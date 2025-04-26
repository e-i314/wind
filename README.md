These are the measurement results from a ground-fixed ultrasonic anemometer, a drone-mounted ultrasonic anemometer, a hot-wire anemometer, and the drone's attitude data.

これは地上に固定された超音波風速計、ドローン搭載の超音波風速計、熱線風速計、そしてドローンの姿勢データを計測した結果です。

1　地上固定型（高さ7mの柱
　センサー ULSA M5B (2025/4/22からULSA PROに変更）https://strvsn.net/ulsa
 　データファイル名　wind_11.txt 等
　　データ詳細

2　ドローン＋超音波風速計
　DJI Marvic 2S
 　ドローンログ名　wx_data_20250125_123211.txt　等
 超音波風速計　TriSonica Mini　https://www.jepico.co.jp/lp_Anemoment.html
　　データファイル名　wx_data_20250125_123211.txt　等
　　ULSA SimpleCSVプロトコル

         #,0,1,359,12.123,24.123,340.123,21.12\r\n
        |A|B|C|-D-|---E--|---F--|---G---|--H--|-I-|

        A: プリアンブル
        B: センサーノードID
        C: 測定有効・無効
        D: 風向(整数) [°]
        E: 風速(浮動小数点数) [m/s]
        F: 機首対気速度(浮動小数点数) [m/s]
        G: 音速(浮動小数点数) [m/s]
        H: 音仮温度(浮動小数点数) [℃]
        I: 改行コード

https://strvsn.notion.site/strvsn/ULSA-d27ee8be0f4940fbbdfbd0df1c06e880#7d444ea0a948479d8fbeb9451d2ad232

3　ドローン＋熱線風速計
　ドローン　DJI Marvic3 Classic
 　ドローンログ名　wx_data_20250125_123211.txt　等
 熱線風速計　HWS-19-ONE　https://www.sg-lab.info/sensors/hws-19-one/
　データファイル名　HWS_20250101.csv 等
  　
