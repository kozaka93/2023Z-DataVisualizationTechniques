import json
import csv
from datetime import datetime 
import os

def to_csv():
    
    directory = 'data\\your_activity_across_facebook\\messages'
    folders = ["archived_threads", "filtered_threads", "inbox"]
    
    exclusions = ["Masz nieodebrane połączenie", "Rozmowa zakończyła się.", "dołączyła do rozmowy.", "dołączył do rozmowy.",
                  "rozpoczęła rozmowę.", "rozpoczął rozmowę.", "opuścił(a) grupę.", "jako administratora grupy.", "innych użytkowników jesteście teraz administratorami.",
                  "zmienił motyw.", "szybką reakcję jako", "ustawił nick użytkownika", "usunęła użytkownika", "usunął użytkownika", "Nadałeś(aś) grupie nazwę"]
    
    with open("data.csv", 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=["conv_id","conv_with","is_group","group_size","sender", "date","content"])
        writer.writeheader()
    
    for folder in folders:
        for ppl_folder in os.listdir(os.path.join(directory, folder)):
            
            conv_id = ppl_folder.split("_")[0]
            
            for filename in os.listdir(os.path.join(directory, folder, ppl_folder)):
                if filename.startswith('message'):
                    data = json.load(open(os.path.join(directory, folder, ppl_folder, filename), "r"))
                    
                    if len(data["participants"]) > 2:
                        group_size = len(data["participants"])
                        is_group = True
                    else:
                        is_group = False
                        group_size = None

                    participants = []
                    for participant in data["participants"]:
                        person = participant["name"].encode('iso-8859-1').decode('utf-8')
                        participants.append(person)
                    conv_with = ", ".join(participants)
                    
                    for message in data["messages"]:
                        try:
                            date = datetime.fromtimestamp(message["timestamp_ms"] / 1000).strftime("%Y-%m-%d %H:%M:%S")
                            sender = message["sender_name"].encode('iso-8859-1').decode('utf-8')
                            content = message["content"].encode('iso-8859-1').decode('utf-8')
                            if (sender + " dodał" in content) or any(e in content for e in exclusions):
                                continue
                        
                            with open("data.csv", 'a', encoding='utf-8') as csv_file:
                                writer = csv.writer(csv_file)
                                writer.writerow([conv_id,conv_with,is_group,group_size,sender,date,content])

                        except KeyError:
                            pass
      
if __name__ == '__main__':
    to_csv()
