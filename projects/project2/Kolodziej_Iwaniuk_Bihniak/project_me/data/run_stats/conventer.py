import csv

import gpxpy
import gpxpy.gpx
from haversine import haversine


def calculate_speeds(gpx_file_path):
    with open(gpx_file_path, "r") as gpx_file:
        gpx = gpxpy.parse(gpx_file)

    speeds = []  # List to hold calculated speeds
    all_points = [
        pt for trk in gpx.tracks for seg in trk.segments for pt in seg.points
    ]  # noqa: E501
    start_time = all_points[0].time

    for i in range(len(all_points) - 1):
        # Calculate the haversine distance between points in kilometers
        lat1, lon1 = all_points[i].latitude, all_points[i].longitude
        lat2, lon2 = all_points[i + 1].latitude, all_points[i + 1].longitude
        distance_km = haversine((lat1, lon1), (lat2, lon2))

        # Calculate time difference in seconds
        time_diff_sec = (
            all_points[i + 1].time - all_points[i].time
        ).total_seconds()  # noqa: E501

        # Calculate speed (km/s) and convert to km/h
        if time_diff_sec > 0:
            speed_km_s = distance_km / time_diff_sec
            speed_km_h = speed_km_s * 3600  # Convert from km/s to km/h
            elapsed_time_sec = int(
                (all_points[i + 1].time - start_time).total_seconds()
            )

            speeds.append((elapsed_time_sec, speed_km_h))

    return speeds


def write_to_csv(speeds, output_file_path):
    with open(output_file_path, "w", newline="") as file:
        writer = csv.writer(file)
        writer.writerow(["Elapsed Time (s)", "Speed (km/h)"])
        for speed in speeds:
            writer.writerow(speed)


# Change 'yourfile.gpx' to the path of your GPX file
speeds_1 = calculate_speeds("data/routes/test1.gpx")
speeds_2 = calculate_speeds("data/routes/test2.gpx")
speeds_3 = calculate_speeds("data/routes/test3.gpx")

# Change 'output.csv' to the desired output CSV file path
write_to_csv(speeds_1, "data/run_stats/data1.csv")
write_to_csv(speeds_2, "data/run_stats/data2.csv")
write_to_csv(speeds_3, "data/run_stats/data3.csv")
