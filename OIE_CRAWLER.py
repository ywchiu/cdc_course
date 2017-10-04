
# coding: utf-8

# In[ ]:

import requests
from bs4 import BeautifulSoup
from datetime import datetime
import re


url = 'http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary'
reportURL = 'http://www.oie.int/wahis_2/public/wahid.php/Reviewreport/Review?page_refer=MapFullEventReport&reportid={}'
payload = {
'disease_type_hidden':'0',
'disease_id_hidden':'15',
'selected_disease_name_hidden':'Highly path. avian influenza (- -) ',
'disease_type':'0',
'disease_id_terrestrial':'15',
'disease_id_aquatic':'-999',
'year':'2017'
}

def getReportDetail(reportID):
    url  = reportURL.format(reportID)
    res  = requests.get(url)
    soup = BeautifulSoup(res.text, 'html.parser')
    country = soup.select('.Rap12-Subtitle')[0].contents[-1]
    
    # 使用prettify 可將 tag 轉換成字串
    dfs  = pandas.read_html(soup.select('.TableFoyers')[0].prettify())
    
    # 將資料轉置
    df = dfs[0].T
    # 將第一列變成欄位名
    df.columns = df.ix[0,]
    # 將第一列刪除
    df = df.drop(0)
    # 增添國家資訊
    df['country'] = country
    
    # 增添抓取時間
    df['search_time'] = datetime.now()
    
    #增添reportID
    df['reportID']  = reportID
    
    # 增添 reportURL
    df['reportURL'] = url
    return df



res = requests.post(url, data=payload)
soup = BeautifulSoup(res.text, 'html.parser')
alinks = soup.select('.vacborder a')

cnt = 0
report_ary = []
for link in alinks:
    if 'MapFullEventReport' in link['href']:
        # method 1: use split and strip
        reportid = link['href'].split(',',)[1].strip(');')
        report_ary.append(getReportDetail(reportid))
        
        # 當取得第五份報告時, 停止, 如果要取得所有報告, 就把下面程式碼拿掉
        cnt = cnt + 1
        if cnt == 5:
            break

reportdf = pandas.concat(report_ary)
reportdf.columns
reportdf = reportdf[(['reportID', 'reportURL', 'search_time', 'Causal agent', 'Date event resolved',
       'Date of confirmation of the event', 'Date of previous occurrence',
       'Date of start of the event', 'Date submitted to OIE',
       'Manifestation of disease', 'Nature of diagnosis',
       'Reason for notification', 'Related reports', 'Report date',
       'Report type', 'Serotype', 'This event pertains to', 'country'])]
reportdf = reportdf[(['reportID', 'reportURL', 'search_time', 'country'])]

import sqlite3 as lite

INSERT_STMT = 'INSERT INTO OIE({}) VALUES({})'
#reportdf['search_time'] = reportdf['search_time'].dt.strftime('%Y-%m-%d')
# 建立連線
con = lite.connect('test.sqlite')
with con:
    # 建立游標 (Cursor)
    cur = con.cursor()
    
    for rec in reportdf.iterrows():
        try:
            data = rec[1].to_dict()

            columns = ', '.join(['"{}"'.format(ele) for ele in data.keys()])
            placeholder = ', '.join(['"{}"'.format(ele) for ele in data.values()])
            SQL = INSERT_STMT.format(columns, placeholder)
            #print(SQL)
            cur.execute(SQL)
        except:
            print(rec[0])
            

