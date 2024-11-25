import json

# JSON 파일 로드
file_path = 'C:/Users/user/local/GitHub/open-data/data/events/9880.json'
with open(file_path, 'r') as file:
    data = json.load(file)

# 변수 초기화
team_possession = {}
total_duration = 0
team_names = set()

# 데이터 분석
for event in data:
    possession_team = event.get("possession_team", {}).get("name", None)
    duration = event.get("duration", 0)
    total_duration += duration

    # 팀 이름 저장
    if possession_team: # 팀 이름이 존재하는 경우
        team_names.add(possession_team)
        team_possession[possession_team] = team_possession.get(possession_team, 0) + duration

# 점유율 계산
possession_percentages = {
    team: (time / total_duration) * 100 for team, time in team_possession.items()
}

# 결과 출력
print("팀 이름:", team_names)
print("볼 점유율:", possession_percentages)

print (total_duration/60)
