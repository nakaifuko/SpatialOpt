estimated_chika5.csv　について

#予測式に用いる変数
i ：切片
x2 ：工業
x3 ：商業
x4 ：住居
x5 ：緊急輸送道路ダミー
x6 ：最寄り駅までの距離

#予測式
y3 = i  + x3 + x4 + x6

#結果
exp(y3)
