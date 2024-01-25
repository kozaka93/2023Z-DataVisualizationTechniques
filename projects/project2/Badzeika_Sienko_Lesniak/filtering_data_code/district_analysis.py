import json
from collections import Counter

import pandas as pd
from find_district import find_district
SPOTIFY_DATA_PATH = 'ZuziaData/StreamingHistory1.json'
GOOGLE_MAPS_DECEMBER_DATA_PATH = 'ZuziaData/2023_DECEMBER.json'
GOOGLE_MAPS_JANUARY_DATA_PATH = 'ZuziaData/2024_JANUARY.json'
TRESHOLD_MS_PLAYED= 30000


f=open(GOOGLE_MAPS_DECEMBER_DATA_PATH)
google_maps_data=json.load(f)
f.close()
f=open(SPOTIFY_DATA_PATH)
spotify=json.load(f)
f.close()
dfSpotify=pd.DataFrame(spotify)
dfSpotify['endTime'] = pd.to_datetime(dfSpotify['endTime']).dt.tz_localize(None)
dfSpotify = dfSpotify[dfSpotify['msPlayed'] > TRESHOLD_MS_PLAYED]
def find_songs(start_time, end_time):
    # Convert start_time and end_time to pandas Timestamp if they aren't already
    start_time = pd.to_datetime(start_time).tz_convert(None).strftime('%Y-%m-%d %H:%M:%S')

    end_time = pd.to_datetime(end_time).tz_convert(None).strftime('%Y-%m-%d %H:%M:%S')


    # Filter dfSpotify based on the time range
    mask = (dfSpotify['endTime'] >= start_time) & (dfSpotify['endTime'] <= end_time)
    relevant_songs = dfSpotify[mask]
    return relevant_songs[['endTime', 'artistName','trackName','msPlayed']].to_dict('records')



def get_place_visited(google_maps_data):
    place_visits = []

    for timeline_object in google_maps_data['timelineObjects']:

        if "placeVisit" in timeline_object:
            activity_segment_json = timeline_object["placeVisit"]

            if not "location" in activity_segment_json or not "latitudeE7" in activity_segment_json["location"]:
                continue

            place_visit = {
                "placeId": activity_segment_json["location"]["placeId"],
                "locationConfidence": activity_segment_json["location"]["locationConfidence"],
                "startTimestamp": activity_segment_json["duration"]["startTimestamp"],
                "endTimestamp": activity_segment_json["duration"]["endTimestamp"],
                "placeVisitImportance": activity_segment_json["placeVisitImportance"],
                "placeVisitType": activity_segment_json["placeVisitType"],
                "latitudeE7": activity_segment_json["location"]["latitudeE7"],
                "longitudeE7": activity_segment_json["location"]["longitudeE7"],
            }

            for optional_field in ["centerLatE7", "centerLngE7"]:
                if optional_field in activity_segment_json:
                    place_visit[optional_field] = activity_segment_json[optional_field]
                else:
                    place_visit[optional_field] = None

            for optional_field in ["name", "address"]:
                if optional_field in activity_segment_json["location"]:
                    place_visit[optional_field] = activity_segment_json["location"][optional_field]
                else:
                    place_visit[optional_field] = None

            place_visits.append(place_visit)
    return place_visits

def get_distr_x_songs(google_maps_data):
    activities_routes = []

    for timeline_object in google_maps_data['timelineObjects']:
        if "activitySegment" in timeline_object:
            activity_segment_json = timeline_object["activitySegment"]

            startTimestamp = pd.Timestamp(activity_segment_json["duration"]["startTimestamp"])
            endTimestamp = pd.Timestamp(activity_segment_json["duration"]["endTimestamp"])

            route = []
            districts = []

            if "waypointPath" in activity_segment_json:
                waypoints = activity_segment_json["waypointPath"]["waypoints"]
                for waypoint in waypoints:
                    lat = waypoint["latE7"] / 1e7
                    lon = waypoint["lngE7"] / 1e7
                    route.append((lat, lon))
                    districts.append(find_district(lat, lon))

            elif "simplifiedRawPath" in activity_segment_json:
                rawPath = activity_segment_json["simplifiedRawPath"]["points"]
                for point in rawPath:
                    lat = point["latE7"] / 1e7
                    lon = point["lngE7"] / 1e7
                    route.append((lat, lon))
                    districts.append(find_district(lat, lon))

            if route:
                counter = Counter(districts)

                most_common_district, count = counter.most_common(1)[0]

                songs = find_songs(startTimestamp, endTimestamp)
                activities_routes.append({
                    "district": most_common_district,
                    "songs": songs
                })

    return activities_routes

place_visits=[]
place_visits+=get_place_visited(google_maps_data)
distr_and_songs = []
distr_and_songs += get_distr_x_songs(google_maps_data)



f=open('GlebData/2024_JANUARY.json')
google_maps_data=json.load(f)
f.close()



place_visits+=get_place_visited(google_maps_data)

distr_and_songs += get_distr_x_songs(google_maps_data)



result = []

for district_info in distr_and_songs:
    district = district_info["district"]
    for song in district_info["songs"]:
        song_with_district = {**song, "district": district}
        result.append(song_with_district)


song_with_district1 = pd.DataFrame(result)





places = pd.DataFrame(place_visits)

places=places.drop(columns=['placeId','centerLatE7', 'centerLngE7','locationConfidence','placeVisitImportance', 'placeVisitType'])





places['startTimestamp'] = pd.to_datetime(places['startTimestamp'],format='mixed').dt.tz_localize(None)
places['endTimestamp'] = pd.to_datetime(places['endTimestamp'],format='mixed').dt.tz_localize(None)


merged_df = pd.merge_asof(dfSpotify.sort_values('endTime'),
                          places.sort_values('startTimestamp'),
                          left_on='endTime',
                          right_on='startTimestamp',
                          direction='backward')



songs_with_districts2 = merged_df[(merged_df['endTime'] >= merged_df['startTimestamp']) & (merged_df['endTime'] <= merged_df['endTimestamp'])]
songs_with_districts2['district']=songs_with_districts2.apply(lambda x: find_district(x['latitudeE7'] / 10000000, x['longitudeE7'] / 10000000), axis=1)
songs_with_districts2.drop(columns=['latitudeE7', 'longitudeE7', 'startTimestamp', 'endTimestamp', 'name', 'address'], inplace=True)

songs_with_districts_final= pd.concat([song_with_district1, songs_with_districts2])
songs_with_districts_final.filter(songs_with_districts_final['district'] != 'No district found')
songs_with_districts_final.to_csv('songs_with_districts_zuzia.csv', index=False)



