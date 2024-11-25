import json
import matplotlib.pyplot as plt
from collections import Counter, defaultdict

def draw_soccer_field(ax, field_dimen=(120, 80)):
    """
    축구 필드 그리기

    매개변수:
    - ax (matplotlib.axes): 필드를 그릴 matplotlib 축
    - field_dimen (tuple): 필드의 크기 (길이, 너비)

    반환값:
    - ax (matplotlib.axes): 축구 필드가 그려진 matplotlib 축
    """
    field_length, field_width = field_dimen

    # 필드 외곽선
    ax.plot([0, 0, field_length, field_length, 0], [0, field_width, field_width, 0, 0], color="black")

    # 센터 라인
    ax.plot([field_length / 2, field_length / 2], [0, field_width], color="black")

    # 센터 서클
    center_circle = plt.Circle((field_length / 2, field_width / 2), 9.15, color="black", fill=False)
    ax.add_patch(center_circle)
    ax.scatter(field_length / 2, field_width / 2, color="black", s=20)  # 센터 스팟

    # 페널티 구역
    ax.plot([0, 16.5, 16.5, 0], [(field_width - 40.3) / 2, (field_width - 40.3) / 2,
                                  (field_width + 40.3) / 2, (field_width + 40.3) / 2], color="black")
    ax.plot([field_length, field_length - 16.5, field_length - 16.5, field_length],
            [(field_width - 40.3) / 2, (field_width - 40.3) / 2,
             (field_width + 40.3) / 2, (field_width + 40.3) / 2], color="black")

    # 골키퍼 구역
    ax.plot([0, 5.5, 5.5, 0], [(field_width - 18.32) / 2, (field_width - 18.32) / 2,
                                (field_width + 18.32) / 2, (field_width + 18.32) / 2], color="black")
    ax.plot([field_length, field_length - 5.5, field_length - 5.5, field_length],
            [(field_width - 18.32) / 2, (field_width - 18.32) / 2,
             (field_width + 18.32) / 2, (field_width + 18.32) / 2], color="black")

    # 골대
    ax.plot([0, -2], [(field_width - 7.32) / 2, (field_width - 7.32) / 2], color="black")
    ax.plot([0, -2], [(field_width + 7.32) / 2, (field_width + 7.32) / 2], color="black")
    ax.plot([field_length, field_length + 2], [(field_width - 7.32) / 2, (field_width - 7.32) / 2], color="black")
    ax.plot([field_length, field_length + 2], [(field_width + 7.32) / 2, (field_width + 7.32) / 2], color="black")

    ax.set_xlim(-5, field_length + 5)
    ax.set_ylim(-5, field_width + 5)
    ax.set_aspect("equal", adjustable="box")
    return ax

def draw_pass_network(events_file_path, lineup_file_path, side):
    """
    홈팀 또는 원정팀의 선발 명단(Starting XI)에 대해 패스 네트워크를 시각화

    매개변수:
    - events_file_path (str): 경기 이벤트 데이터를 포함한 JSON 파일 경로
    - lineup_file_path (str): 라인업 데이터를 포함한 JSON 파일 경로
    - side (str): "home" 또는 "away"를 지정해 특정 팀 선택

    반환값:
    - None: 패스 네트워크 맵을 화면에 출력
    """
    # side 입력값 검증
    if side.lower() not in ["home", "away"]:
        raise ValueError('Invalid side input. The "side" parameter must be either "home" or "away".')

    # 데이터 로드
    with open(events_file_path, 'r', encoding='utf-8') as f:
        events_data = json.load(f)
    with open(lineup_file_path, 'r', encoding='utf-8') as f:
        lineup_data = json.load(f)

    # 팀과 선발 명단 확인
    lineup_info = lineup_data[0 if side.lower() == "home" else 1]
    team_name = lineup_info["team_name"]
    starting_players = {
        player["player_name"] for player in lineup_info["lineup"]
        if any(position["start_reason"] == "Starting XI" for position in player.get("positions", []))
    }

    # 선택된 팀과 선수의 패스 필터링
    team_passes = [
        event for event in events_data
        if event["type"]["name"] == "Pass" and event["team"]["name"] == team_name
    ]

    # 각 선수의 패스 위치 수집
    player_positions = defaultdict(list)
    pass_counter = Counter()
    for event in team_passes:
        passer = event["player"]["name"]
        recipient = event["pass"].get("recipient", {}).get("name")
        if passer in starting_players:
            player_positions[passer].append(event["location"])
        if recipient in starting_players:
            player_positions[recipient].append(event["pass"]["end_location"])
        if passer in starting_players and recipient in starting_players:
            pass_counter[(passer, recipient)] += 1

    # 선수별 평균 위치 계산
    average_positions = {
        player: [sum(coord) / len(coord) for coord in zip(*positions)]
        for player, positions in player_positions.items()
    }

    # 각 선수의 총 패스 수 계산 (보낸 패스 + 받은 패스)
    pass_counts = Counter()
    for (passer, recipient), count in pass_counter.items():
        pass_counts[passer] += count
        pass_counts[recipient] += count

    # 축구 필드와 패스 네트워크 그리기
    fig, ax = plt.subplots(figsize=(12, 8))
    ax = draw_soccer_field(ax)

    # 패스 연결선 그리기
    for (passer, recipient), count in pass_counter.items():
        if passer in average_positions and recipient in average_positions:
            start_x, start_y = average_positions[passer]
            end_x, end_y = average_positions[recipient]
            ax.plot(
                [start_x, end_x], [start_y, end_y],
                color="blue", linewidth=count / 2, alpha=0.7
            ) 

    # 선수 위치와 이름 표시
    for player, (x, y) in average_positions.items():
        node_size = pass_counts[player] * 5
        ax.scatter(x, y, c="red", s=node_size, edgecolor="black", zorder=5)
        ax.text(x, y + 2, player, fontsize=10, ha="center", zorder=6)

    # 최종 그래프 설정
    plt.title(f"{team_name} Pass Network (Starting XI)", fontsize=14)
    plt.axis("off")
    plt.show()

draw_pass_network("C:/Users/user/local/GitHub/open-data/data/events/3773457.json",
                  "C:/Users/user/local/GitHub/open-data/data/lineups/3773457.json",
                  "home")
