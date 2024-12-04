import os
import json
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import ttest_ind, mannwhitneyu
from statsmodels.stats.multitest import multipletests
from mplsoccer import Pitch

# %%
# matches json 파일속에서 event를 확인할 match_id 추출하기

folder_path = "C:/Users/insung/Desktop/5-1/R프로그래밍/data/matches"
json_files = [file for file in os.listdir(folder_path) if file.endswith('.json')]

# match_id를 추출할 json 파일이름 리스트
print(f"Total match JSON files: {len(json_files)}")
print(json_files)
# %%
# json 파일 리스트를 for 구문으로 돌면서 La Liga match_id 수집
match_id_list = []
matches_info = []  # 모든 경기 정보를 저장할 리스트
for json_file in json_files:
    file_path = os.path.join(folder_path, json_file)
    
    try:
        df = pd.read_json(file_path)
        match_id_info = df['match_id'].tolist()
        matches_info.extend(df.to_dict(orient='records'))
        print(f'Processing file: {json_file} with {len(match_id_info)} matches')    

        match_id_list.extend(match_id_info)
    except Exception as e:
        print(f"Error processing file {json_file}: {e}")

# %%
# matches_info를 DataFrame으로 변환
matches_df = pd.json_normalize(matches_info)

# %%
# 전체 경기 수와 Barcelona가 치른 경기 수 및 비율 계산
def calculate_barcelona_matches(matches_df, team_name="Barcelona"):
    total_matches = len(matches_df)
    barca_matches = matches_df[
        (matches_df['home_team.home_team_name'] == team_name) | 
        (matches_df['away_team.away_team_name'] == team_name)
    ]
    barca_match_count = len(barca_matches)
    ratio = (barca_match_count / total_matches) * 100 if total_matches > 0 else 0
    print(f"\n전체 경기 수: {total_matches}")
    print(f"Barcelona가 치른 경기 수: {barca_match_count}")
    print(f"Barcelona 경기 비율: {ratio:.2f}%")
    return barca_matches

# %%
# La Liga 매치 이벤트 파일 추출
laliga_events_path = "C:/Users/insung/Desktop/5-1/R프로그래밍/기말프로젝트/events"
laliga_matches_path = "C:/Users/insung/Desktop/5-1/R프로그래밍/data/matches"  # 경기 정보가 저장된 경로
laliga_events_files = []
for file in os.listdir(laliga_events_path):
    if any(file.endswith(f"{match_id}.json") for match_id in match_id_list):
        laliga_events_files.append(file)

# %%
print(f"\nLa Liga JSON files count: {len(laliga_events_files)}")
print("La Liga JSON files:", laliga_events_files)

#%% # 전처리용 함수

# 'formation' 정보 추출 함수 정의
def extract_formation(tactics):
    if isinstance(tactics, dict):
        return tactics.get('formation', None)
    else:
        return None

# 'location' 데이터 처리 함수 정의
def handle_location(x):
    if isinstance(x, list):
        return x  
    else:
        return None

# %%
# 한개의 경기에 대해 분석
# file_name = laliga_events_files[1]
# file_path = os.path.join(laliga_events_path, file_name)
# df = pd.read_json(file_path)

# %%
def assign_zone(x_coordinate, field_length=120, direction=1):
    """
    X 좌표를 기반으로 경기장의 구역을 할당합니다.
    
    Args:
    - x_coordinate (float): X 좌표값
    - field_length (float): 경기장의 전체 길이 (기본값: 120)
    - direction (int): 팀의 공격 방향 (1: 좌에서 우, -1: 우에서 좌)
    
    Returns:
    - zone (str): 'Defensive', 'Midfield', 'Attacking' 중 하나
    """
    if direction == 1:
        if x_coordinate <= field_length / 3:
            return 'Defensive'
        elif x_coordinate <= 2 * field_length / 3:
            return 'Midfield'
        else:
            return 'Attacking'
    elif direction == -1:
        if x_coordinate >= 2 * field_length / 3:
            return 'Defensive'
        elif x_coordinate >= field_length / 3:
            return 'Midfield'
        else:
            return 'Attacking'
    else:
        raise ValueError("Direction must be either 1 (left to right) or -1 (right to left).")

def process_pass_events(events, home_team, away_team, direction_home=1, direction_away=-1):
    """
    패스 이벤트를 필터링하고, 전반전과 후반전으로 구분하여 팀별로 구역을 할당한 DataFrame을 반환합니다.
    
    Args:
    - events (list): 이벤트 리스트
    - home_team (str): 홈팀 이름
    - away_team (str): 어웨이팀 이름
    - direction_home (int): 홈팀의 공격 방향 (1: 좌에서 우, -1: 우에서 좌)
    - direction_away (int): 어웨이팀의 공격 방향 (1: 좌에서 우, -1: 우에서 좌)
    
    Returns:
    - home_first_half_df (pd.DataFrame): 홈팀 전반전 패스 이벤트 데이터
    - home_second_half_df (pd.DataFrame): 홈팀 후반전 패스 이벤트 데이터
    - away_first_half_df (pd.DataFrame): 어웨이팀 전반전 패스 이벤트 데이터
    - away_second_half_df (pd.DataFrame): 어웨이팀 후반전 패스 이벤트 데이터
    """
    pass_events = [
        event for event in events 
        if event.get('type', {}).get('name') == 'Pass'
        and 'location' in event 
        and isinstance(event['location'], list) 
        and len(event['location']) == 2
    ]
    
    data_home_first = []
    data_home_second = []
    data_away_first = []
    data_away_second = []
    
    for event in pass_events:
        possession_team = event.get('possession_team', {}).get('name', None)
        location = event.get('location', None)
        period = event.get('period', None)
        if possession_team and location and period in [1, 2]:
            x, y = location
            # 팀의 공격 방향에 따라 구역 할당
            if possession_team == home_team:
                direction = direction_home
            elif possession_team == away_team:
                direction = direction_away
            else:
                continue  # 기타 팀의 경우 무시
                
            zone = assign_zone(x, field_length=120, direction=direction)
            entry = {
                'team': possession_team,
                'x': x,
                'y': y,
                'zone': zone
            }
            if possession_team == home_team:
                if period == 1:
                    data_home_first.append(entry)
                elif period == 2:
                    data_home_second.append(entry)
            elif possession_team == away_team:
                if period == 1:
                    data_away_first.append(entry)
                elif period == 2:
                    data_away_second.append(entry)
    
    home_first_half_df = pd.DataFrame(data_home_first)
    home_second_half_df = pd.DataFrame(data_home_second)
    away_first_half_df = pd.DataFrame(data_away_first)
    away_second_half_df = pd.DataFrame(data_away_second)
    
    return home_first_half_df, home_second_half_df, away_first_half_df, away_second_half_df

def plot_heatmap(home_first_df, home_second_df, away_first_df, away_second_df, home_team, away_team):
    """
    홈팀과 어웨이팀의 전반전 및 후반전 점유 히트맵을 2x2 서브플롯으로 시각화합니다.
    
    Args:
    - home_first_df (pd.DataFrame): 홈팀 전반전 패스 이벤트 데이터
    - home_second_df (pd.DataFrame): 홈팀 후반전 패스 이벤트 데이터
    - away_first_df (pd.DataFrame): 어웨이팀 전반전 패스 이벤트 데이터
    - away_second_df (pd.DataFrame): 어웨이팀 후반전 패스 이벤트 데이터
    - home_team (str): 홈팀 이름
    - away_team (str): 어웨이팀 이름
    """
    # 피치 설정
    pitch = Pitch(pitch_type='statsbomb', pitch_length=120, pitch_width=80, line_color='#757575')
    
    # 서브플롯 생성: 2행 2열
    fig, axes = plt.subplots(nrows=2, ncols=2, figsize=(24, 16))
    
    # 전반전 홈팀 히트맵
    pitch.draw(ax=axes[0, 0])
    if not home_first_df.empty:
        sns.kdeplot(
            x=home_first_df['x'], y=home_first_df['y'],
            fill=True,
            alpha=0.6, cmap='Reds',
            bw_adjust=1.5,
            ax=axes[0, 0],
            label=f'{home_team} 전반전 점유'
        )
    axes[0, 0].set_title(f"{home_team} 전반전 점유 히트맵", fontsize=16)
    handles, labels = axes[0, 0].get_legend_handles_labels()
    if handles:
        axes[0, 0].legend(handles, labels, loc='upper right', fontsize=12)
    
    # 전반전 어웨이팀 히트맵
    pitch.draw(ax=axes[0, 1])
    if not away_first_df.empty:
        sns.kdeplot(
            x=away_first_df['x'], y=away_first_df['y'],
            fill=True,
            alpha=0.6, cmap='Blues',
            bw_adjust=1.5,
            ax=axes[0, 1],
            label=f'{away_team} 전반전 점유'
        )
    axes[0, 1].set_title(f"{away_team} 전반전 점유 히트맵", fontsize=16)
    handles, labels = axes[0, 1].get_legend_handles_labels()
    if handles:
        axes[0, 1].legend(handles, labels, loc='upper right', fontsize=12)
    
    # 후반전 홈팀 히트맵
    pitch.draw(ax=axes[1, 0])
    if not home_second_df.empty:
        sns.kdeplot(
            x=home_second_df['x'], y=home_second_df['y'],
            fill=True,
            alpha=0.6, cmap='Reds',
            bw_adjust=1.5,
            ax=axes[1, 0],
            label=f'{home_team} 후반전 점유'
        )
    axes[1, 0].set_title(f"{home_team} 후반전 점유 히트맵", fontsize=16)
    handles, labels = axes[1, 0].get_legend_handles_labels()
    if handles:
        axes[1, 0].legend(handles, labels, loc='upper right', fontsize=12)
    
    # 후반전 어웨이팀 히트맵
    pitch.draw(ax=axes[1, 1])
    if not away_second_df.empty:
        sns.kdeplot(
            x=away_second_df['x'], y=away_second_df['y'],
            fill=True,
            alpha=0.6, cmap='Blues',
            bw_adjust=1.5,
            ax=axes[1, 1],
            label=f'{away_team} 후반전 점유'
        )
    axes[1, 1].set_title(f"{away_team} 후반전 점유 히트맵", fontsize=16)
    handles, labels = axes[1, 1].get_legend_handles_labels()
    if handles:
        axes[1, 1].legend(handles, labels, loc='upper right', fontsize=12)
    
    # 전체 제목 추가
    fig.suptitle(f"{home_team} vs {away_team} 점유 히트맵 (전반전 & 후반전)", fontsize=20)
    
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])  # 전체 제목을 위해 레이아웃 조정
    plt.show()

def calculate_spatial_possession(home_df, away_df):
    """
    홈팀과 어웨이팀의 구역별 점유율을 계산합니다.
    
    Args:
    - home_df (pd.DataFrame): 홈팀 패스 이벤트 데이터
    - away_df (pd.DataFrame): 어웨이팀 패스 이벤트 데이터
    
    Returns:
    - spatial_df (pd.DataFrame): 구역별 점유율 데이터
    """
    spatial_counts = {
        'Zone': ['Defensive', 'Midfield', 'Attacking'],
        'Home': [
            len(home_df[home_df['zone'] == 'Defensive']),
            len(home_df[home_df['zone'] == 'Midfield']),
            len(home_df[home_df['zone'] == 'Attacking'])
        ],
        'Away': [
            len(away_df[away_df['zone'] == 'Defensive']),
            len(away_df[away_df['zone'] == 'Midfield']),
            len(away_df[away_df['zone'] == 'Attacking'])
        ]
    }
    
    spatial_df = pd.DataFrame(spatial_counts)
    spatial_df['Home_Perc'] = (spatial_df['Home'] / spatial_df['Home'].sum()) * 100 if spatial_df['Home'].sum() > 0 else 0
    spatial_df['Away_Perc'] = (spatial_df['Away'] / spatial_df['Away'].sum()) * 100 if spatial_df['Away'].sum() > 0 else 0
    
    return spatial_df

def plot_spatial_possession(spatial_df, home_team, away_team, period):
    """
    구역별 점유율을 바 차트로 시각화합니다.
    
    Args:
    - spatial_df (pd.DataFrame): 구역별 점유율 데이터
    - home_team (str): 홈팀 이름
    - away_team (str): 어웨이팀 이름
    - period (str): '전반전' 또는 '후반전'
    """
    fig, ax = plt.subplots(figsize=(10, 6))
    
    bar_width = 0.35
    index = np.arange(len(spatial_df['Zone']))
    
    bars1 = ax.bar(index, spatial_df['Home_Perc'], bar_width, label=f'{home_team}', color='red', alpha=0.7)
    bars2 = ax.bar(index + bar_width, spatial_df['Away_Perc'], bar_width, label=f'{away_team}', color='blue', alpha=0.7)
    
    ax.set_xlabel('Zone')
    ax.set_ylabel('Possession Percentage')
    ax.set_title(f'{period} Spatial Possession Percentage by Zone')
    ax.set_xticks(index + bar_width / 2)
    ax.set_xticklabels(spatial_df['Zone'])
    ax.legend()
    
    # 값 표시
    for bar in bars1 + bars2:
        height = bar.get_height()
        ax.annotate(f'{height:.1f}%',
                    xy=(bar.get_x() + bar.get_width() / 2, height),
                    xytext=(0, 3),  # 3 포인트 수직 오프셋
                    textcoords="offset points",
                    ha='center', va='bottom')
    
    plt.show()

def find_match_by_id(folder_path, target_match_id):
    """
    특정 폴더에 있는 모든 JSON 파일을 순회하며 주어진 match_id와 일치하는 데이터를 찾음.
    
    Args:
    - folder_path: JSON 파일이 저장된 폴더 경로
    - target_match_id: 찾고자 하는 match_id 값
    
    Returns:
    - match_info: match_id와 일치하는 경기 데이터 (없으면 None)
    """
    for file_name in os.listdir(folder_path):
            file_path = os.path.join(folder_path, file_name)
            with open(file_path, 'r', encoding='utf-8') as file:
                try:
                    data = json.load(file)
                    for match in data:
                        if match.get("match_id") == target_match_id:
                            return match 
                except Exception as e:
                    print(f"Error reading file {file_name}: {e}")
    
    return None  

def analyze_match(match_info, file_name=None):
    """
    주어진 경기 정보에 대한 분석 수행.
    
    Args:
    - match_info: 특정 match_id에 해당하는 경기 정보
    - file_name: 해당 경기 정보가 포함된 파일 이름 (optional, for error logging)
    
    Returns:
    - home_team_name, away_team_name: 홈팀 어웨이팀 이름
    """
    if match_info is None:
        print(f"해당 match_id에 대한 경기 정보를 찾을 수 없습니다. (File: {file_name})")
        return None, None
    try:
        home_team_name = match_info['home_team']['home_team_name']
        away_team_name = match_info['away_team']['away_team_name']
    except KeyError as e:
        print(f"Missing team name key {e} in match_info. (File: {file_name})")
        return None, None
    # 기본 정보 출력
    try:
        print(f'\nMatch ID: {match_info["match_id"]} 경기에 대해서 분석을 진행하고자 합니다..')
        print(f"Date: {match_info['match_date']}")
        print(f"Competition: {match_info['competition']['competition_name']}")
        print(f"Home Team: {home_team_name} (Score: {match_info['home_score']})")
        print(f"Away Team: {away_team_name} (Score: {match_info['away_score']})")
    except KeyError as e:
        print(f"Missing key {e} in match_info. (File: {file_name})")
    
    # Managers 정보 처리
    try:
        home_manager = match_info['home_team']['managers'][0]
        away_manager = match_info['away_team']['managers'][0]
        print(f"Home Manager: {home_manager['name']} ({home_manager['country']['name']})")
        print(f"Away Manager: {away_manager['name']} ({away_manager['country']['name']})")
    except KeyError as e:
        print(f"Missing managers key {e} in match_info. (File: {file_name})")
    except IndexError as e:
        print(f"Managers list is empty or insufficient: {e}. (File: {file_name})")
    
    # Stadium 정보 처리
    try:
        stadium_name = match_info['stadium']['name']
        stadium_country = match_info['stadium']['country']['name']
        print(f"Stadium: {stadium_name} ({stadium_country})")
    except KeyError as e:
        print(f"Missing stadium key {e} in match_info. (File: {file_name})")
    
    return home_team_name, away_team_name

#%%
def main():
    # 1. 전체 경기 수와 Barcelona가 치른 경기 수 및 비율 계산
    barca_matches_df = calculate_barcelona_matches(matches_df, team_name="Barcelona")
    
    # 2. Barcelona가 치른 경기 중 승리와 패배한 경기 분류
    barca_matches_df = barca_matches_df.reset_index(drop=True)
    barca_results = []
    for idx, match in barca_matches_df.iterrows():
        home_team = match['home_team.home_team_name']
        away_team = match['away_team.away_team_name']
        home_score = match['home_score']
        away_score = match['away_score']
        
        if home_team == "Barcelona":
            barca_score = home_score
            opponent_score = away_score
            if barca_score > opponent_score:
                result = 'Win'
            elif barca_score < opponent_score:
                result = 'Loss'
            else:
                result = 'Draw'
        elif away_team == "Barcelona":
            barca_score = away_score
            opponent_score = home_score
            if barca_score > opponent_score:
                result = 'Win'
            elif barca_score < opponent_score:
                result = 'Loss'
            else:
                result = 'Draw'
        else:
            result = 'Other'  # Barcelona가 아닌 경기

        barca_results.append(result)
    
    barca_matches_df['Result'] = barca_results
    barca_played_matches = barca_matches_df[barca_matches_df['Result'].isin(['Win', 'Loss'])]
    barca_wins = barca_played_matches[barca_played_matches['Result'] == 'Win']
    barca_losses = barca_played_matches[barca_played_matches['Result'] == 'Loss']
    
    total_played = len(barca_played_matches)
    total_wins = len(barca_wins)
    total_losses = len(barca_losses)
    win_ratio = (total_wins / total_played) * 100 if total_played > 0 else 0
    loss_ratio = (total_losses / total_played) * 100 if total_played > 0 else 0
    
    print(f"\nBarcelona가 승리한 경기 수: {total_wins}")
    print(f"Barcelona가 패배한 경기 수: {total_losses}")
    print(f"승리 비율: {win_ratio:.2f}%")
    print(f"패배 비율: {loss_ratio:.2f}%")
    
    # 3. 승리와 패배한 경기의 match_id 리스트 추출
    win_match_ids = barca_wins['match_id'].tolist()
    loss_match_ids = barca_losses['match_id'].tolist()
    
    # 4. 승리와 패배한 경기의 이벤트 파일 리스트 생성
    win_events_files = [file for file in laliga_events_files if any(file.endswith(f"{match_id}.json") for match_id in win_match_ids)]
    loss_events_files = [file for file in laliga_events_files if any(file.endswith(f"{match_id}.json") for match_id in loss_match_ids)]
    
    print(f"\nBarcelona가 승리한 경기의 이벤트 파일 수: {len(win_events_files)}")
    print(f"Barcelona가 패배한 경기의 이벤트 파일 수: {len(loss_events_files)}")
    
    # 5. 승리한 경기와 패배한 경기에 대한 통계 분석
    def analyze_matches(event_files, result_type):
        """
        주어진 이벤트 파일 리스트에 대해 구역별 점유율을 계산하고 반환합니다.
        
        Args:
        - event_files (list): 이벤트 파일 리스트
        - result_type (str): 'Win' 또는 'Loss'
        
        Returns:
        - possession_df (pd.DataFrame): 각 경기의 구역별 점유율을 포함한 데이터프레임
        """
        possession_records = []
        error_files = []  # 에러가 발생한 파일명을 저장할 리스트
        for file in event_files:
            file_name = file
            try:
                target_match_id = int(file_name.split('.')[0])
            except ValueError:
                print(f"Invalid match_id in file name: {file_name}")
                error_files.append(file_name)
                continue

            # match_id로 경기 정보 검색
            match_info = find_match_by_id(laliga_matches_path, target_match_id)
            if match_info is None:
                print(f"Match info not found for match_id: {target_match_id} in file: {file_name}")
                error_files.append(file_name)
                continue

            try:
                home_team, away_team = analyze_match(match_info, file_name=file_name)
            except KeyError as e:
                print(f"KeyError in file {file_name}: {e}")
                error_files.append(file_name)
                continue
            except Exception as e:
                print(f"Error analyzing match in file {file_name}: {e}")
                error_files.append(file_name)
                continue

            if not home_team or not away_team:
                print(f"Skipping file {file_name} due to missing team names.")
                error_files.append(file_name)
                continue

            file_path = os.path.join(laliga_events_path, file_name)
            try:
                with open(file_path, 'r', encoding='utf-8') as file_handle:
                    events = json.load(file_handle)
            except Exception as e:
                print(f"Error reading events from file {file_name}: {e}")
                error_files.append(file_name)
                continue

            # 홈팀과 어웨이팀의 공격 방향 설정 (예: 홈팀은 좌에서 우, 어웨이팀은 우에서 좌)
            direction_home = 1  # 홈팀은 좌에서 우로 공격
            direction_away = -1  # 어웨이팀은 우에서 좌로 공격

            # 패스 이벤트 처리 (전반전과 후반전으로 구분)
            home_first_df, home_second_df, away_first_df, away_second_df = process_pass_events(
                events, home_team, away_team, direction_home, direction_away
            )
            
            # 전체 경기의 구역별 점유율 계산 (전반전과 후반전 합산)
            combined_home_df = pd.concat([home_first_df, home_second_df], ignore_index=True)
            combined_away_df = pd.concat([away_first_df, away_second_df], ignore_index=True)
            spatial_df = calculate_spatial_possession(combined_home_df, combined_away_df)
            
            # 구역별 점유율을 기록
            possession_record = {'match_id': target_match_id}
            for _, row in spatial_df.iterrows():
                possession_record[f"{row['Zone']}_Home_Perc"] = row['Home_Perc']
                possession_record[f"{row['Zone']}_Away_Perc"] = row['Away_Perc']
            possession_records.append(possession_record)
        
        possession_df = pd.DataFrame(possession_records)
        possession_df['Result'] = result_type

        if error_files:
            print(f"\nErrors occurred in the following files: {error_files}")
        return possession_df

    
    print("\n승리한 경기의 구역별 점유율 계산 중...")
    win_possession_df = analyze_matches(win_events_files, 'Win')
    print(f"승리 경기의 구역별 점유율 데이터프레임:\n{win_possession_df.head()}")
    
    print("\n패배한 경기의 구역별 점유율 계산 중...")
    loss_possession_df = analyze_matches(loss_events_files, 'Loss')
    print(f"패배 경기의 구역별 점유율 데이터프레임:\n{loss_possession_df.head()}")
    
    # 6. 통계 분석을 위해 승리와 패배 경기 데이터를 결합
    possession_combined_df = pd.concat([win_possession_df, loss_possession_df], ignore_index=True)
    
    # 7. 구역별 점유율 차이에 대한 통계 검정
    def perform_statistical_tests(df, zones=['Defensive', 'Midfield', 'Attacking']):
        """
        구역별 점유율에 대해 승리와 패배 경기 간의 통계적 차이를 검정합니다.
        
        Args:
        - df (pd.DataFrame): 구역별 점유율 데이터프레임
        - zones (list): 분석할 구역 리스트
        
        Returns:
        - results_df (pd.DataFrame): 각 구역에 대한 통계 검정 결과
        """
        results = []
        for zone in zones:
            win_col = f"{zone}_Home_Perc"
            loss_col = f"{zone}_Home_Perc"
            
            # 승리 경기의 구역별 점유율
            win_data = df[df['Result'] == 'Win'][win_col].dropna()
            # 패배 경기의 구역별 점유율
            loss_data = df[df['Result'] == 'Loss'][loss_col].dropna()
            
            # 데이터 수 확인
            if len(win_data) < 3 or len(loss_data) < 3:
                print(f"Not enough data for {zone} zone to perform statistical test.")
                continue
            
            # 정규성 확인 (Shapiro-Wilk 테스트)
            from scipy.stats import shapiro
            try:
                _, p_win = shapiro(win_data)
                _, p_loss = shapiro(loss_data)
                normal = (p_win > 0.05) and (p_loss > 0.05)
            except Exception as e:
                print(f"Error performing Shapiro-Wilk test for {zone} zone: {e}")
                normal = False
            
            # 등분산성 확인 (Levene 테스트)
            from scipy.stats import levene
            try:
                _, p_levene = levene(win_data, loss_data)
                equal_var = p_levene > 0.05
            except Exception as e:
                print(f"Error performing Levene test for {zone} zone: {e}")
                equal_var = False
            
            # 검정 선택
            if normal:
                # T-검정
                stat, p_value = ttest_ind(win_data, loss_data, equal_var=equal_var)
                test_type = 'T-Test'
            else:
                # Mann-Whitney U 검정
                stat, p_value = mannwhitneyu(win_data, loss_data, alternative='two-sided')
                test_type = 'Mann-Whitney U'
            
            results.append({
                'Zone': zone,
                'Test': test_type,
                'Statistic': stat,
                'p-value': p_value
            })
        
        results_df = pd.DataFrame(results)
        # 다중 비교 보정 (Bonferroni)
        if not results_df.empty:
            _, corrected_pvals, _, _ = multipletests(results_df['p-value'], method='bonferroni')
            results_df['Corrected p-value'] = corrected_pvals
            results_df['Significant'] = results_df['Corrected p-value'] < 0.05
        return results_df

    print("\n구역별 점유율 차이에 대한 통계 검정 수행 중...")
    stat_results = perform_statistical_tests(possession_combined_df)
    print(f"\n통계 검정 결과:\n{stat_results}")
    
    # 8. 통계 검정 결과 시각화
    def plot_statistical_results(results_df):
        """
        통계 검정 결과를 시각화합니다.
        
        Args:
        - results_df (pd.DataFrame): 통계 검정 결과 데이터프레임
        """
        if results_df.empty:
            print("No statistical test results to plot.")
            return
        
        fig, ax = plt.subplots(figsize=(10, 6))
        sns.barplot(x='Zone', y='p-value', hue='Test', data=results_df, palette='viridis', ax=ax)
        
        # 유의미한 p-value 표시
        for idx, row in results_df.iterrows():
            if row['Significant']:
                ax.text(idx, row['p-value'], '*', color='red', ha='center', va='bottom', fontsize=20)
        
        ax.axhline(0.05, color='red', linestyle='--')
        ax.set_title('구역별 점유율 차이에 대한 통계 검정 결과 (Bonferroni 보정)', fontsize=16)
        ax.set_ylabel('p-value')
        ax.set_xlabel('Zone')
        ax.legend(title='Test Type')
        plt.show()
    
    plot_statistical_results(stat_results)
    
    # 9. 승리 경기와 패배 경기의 구역별 점유율 비교 박스 플롯
    def compare_possession(df, zone):
        """
        특정 구역에 대한 승리와 패배 경기의 점유율을 비교하는 박스 플롯을 생성합니다.
        
        Args:
        - df (pd.DataFrame): 구역별 점유율 데이터프레임
        - zone (str): 비교할 구역
        """
        plt.figure(figsize=(8, 6))
        sns.boxplot(x='Result', y=f"{zone}_Home_Perc", data=df, palette=['green', 'orange'])
        sns.swarmplot(x='Result', y=f"{zone}_Home_Perc", data=df, color='black', alpha=0.5)
        plt.title(f"{zone} Zone Possession Percentage: Win vs Loss")
        plt.ylabel('Possession Percentage')
        plt.xlabel('Match Result')
        plt.show()
    
    zones = ['Defensive', 'Midfield', 'Attacking']
    for zone in zones:
        compare_possession(possession_combined_df, zone)
    
    # 10. 모든 팀의 경기 참여 수 및 비율 계산
    def calculate_all_teams_participation(matches_df):
        """
        모든 팀의 경기 참여 수와 비율을 계산하고 출력합니다.
        
        Args:
        - matches_df (pd.DataFrame): 전체 경기 데이터프레임
        
        Returns:
        - teams_participation_df (pd.DataFrame): 팀별 경기 참여 수와 비율을 포함한 데이터프레임
        """
        # 홈팀과 어웨이팀 이름 추출
        home_teams = matches_df['home_team.home_team_name']
        away_teams = matches_df['away_team.away_team_name']
        
        # 모든 팀 이름을 하나의 시리즈로 결합
        all_teams = pd.concat([home_teams, away_teams])
        
        # 팀별 경기 수 계산
        team_counts = all_teams.value_counts().reset_index()
        team_counts.columns = ['Team', 'Match_Count']
        
        # 전체 경기 수 (각 경기는 두 팀이 참가하므로 전체 팀 경기 수는 2 * total_matches)
        total_team_matches = 2 * len(matches_df)
        
        # 팀별 경기 비율 계산
        team_counts['Match_Ratio'] = (team_counts['Match_Count'] / total_team_matches) * 100
        
        # 정렬
        team_counts = team_counts.sort_values(by='Match_Count', ascending=False).reset_index(drop=True)
        
        print("\n모든 팀의 경기 참여 수 및 비율:")
        print(team_counts)
        
        return team_counts

    teams_participation_df = calculate_all_teams_participation(matches_df)
    
    # 11. 모든 팀의 경기 참여 비율 시각화
    def plot_teams_participation(teams_df, top_n=20):
        """
        상위 N개의 팀에 대한 경기 참여 비율을 시각화합니다.
        
        Args:
        - teams_df (pd.DataFrame): 팀별 경기 참여 데이터프레임
        - top_n (int): 상위 N개 팀을 시각화할지 여부
        """
        # 상위 N개 팀 선택
        if top_n:
            teams_to_plot = teams_df.head(top_n)
        else:
            teams_to_plot = teams_df
        
        plt.figure(figsize=(12, 8))
        sns.barplot(x='Match_Ratio', y='Team', data=teams_to_plot, palette='viridis')
        plt.title(f'상위 {top_n} 팀의 경기 참여 비율', fontsize=16)
        plt.xlabel('Match Ratio (%)')
        plt.ylabel('Team')
        plt.show()
    
    # 전체 팀이 많을 경우 상위 20개 팀만 시각화
    plot_teams_participation(teams_participation_df, top_n=5)
    
    # 만약 모든 팀을 시각화하고 싶다면 top_n=None을 설정하세요.
    # plot_teams_participation(teams_participation_df, top_n=None)

if __name__ == "__main__":
    main()
