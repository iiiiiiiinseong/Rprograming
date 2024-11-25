import json

def explore_json(file_path):
    def explore_item(item, indent=0):
        """Recursively explores nested structures."""
        if isinstance(item, dict):
            for key, value in item.items():
                print(" " * indent + f"Key: {key}")
                explore_item(value, indent + 2)
        elif isinstance(item, list):
            print(" " * indent + f"List of {len(item)} items:")
            if len(item) > 0:
                explore_item(item[0], indent + 2)
        else:
            print(" " * indent + f"Value: {item}")

    # Load the JSON file with explicit encoding
    with open(file_path, 'r', encoding='utf-8') as file:
        data = json.load(file)
    
    if isinstance(data, list):
        print(f"Root is a list with {len(data)} items.")
        explore_item(data[0])
    elif isinstance(data, dict):
        print("Root is a dictionary.")
        explore_item(data)
    else:
        print("Unsupported JSON structure.")

# JSON 파일 경로를 설정하고 실행
json_file_path = 'C:/Users/user/local/GitHub/open-data/data/events/3773457.json'
explore_json(json_file_path)
