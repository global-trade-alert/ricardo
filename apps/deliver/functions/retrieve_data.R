retrieve_data = function(){
  
  gta_sql_kill_connections()
  
  database <<- "ricardomain"
  
  gta_sql_pool_open(db.title="ricardomainclone",
                    db.host = "gta-ricardo-dev.cp7esvs8xwum.eu-west-1.rds.amazonaws.com",
                    db.name = 'ricardomainclone',
                    db.user = 'gtaricardodev',
                    db.password = 'nC6okGiDKEcFV36rKsykeE9HXbfphgAH6',
                    table.prefix = "bt_")
  
  ricardo.data=gta_sql_get_value("SELECT bhl.hint_id, bhl.hint_state_id, bhb.bid, gjl.jurisdiction_name, b2al.assessment_name, bht.hint_title, bht.hint_description, butl.url_type_name, bul.url, bhl.acting_agency
                               FROM bt_hint_log bhl
                               JOIN bt_hint_bid bhb
                               ON bhl.hint_id = bhb.hint_id
                               JOIN bt_hint_jurisdiction bhj
                               ON bhl.hint_id = bhj.hint_id
                               JOIN gta_jurisdiction_list gjl
                               ON bhj.jurisdiction_id = gjl.jurisdiction_id
                               JOIN b221_hint_assessment b2ha
                               ON bhl.hint_id = b2ha.hint_id
                               JOIN b221_assessment_list b2al
                               ON b2ha.assessment_id=b2al.assessment_id
                               JOIN bt_hint_text bht
                               ON bhl.hint_id = bht.hint_id 
                               JOIN bt_hint_url bhu
                               ON bhl.hint_id = bhu.hint_id
                               JOIN bt_url_log bul
                               ON bhu.url_id = bul.url_id
                               JOIN bt_url_type_list butl
                               ON bhu.url_type_id = butl.url_type_id
                               WHERE bhl.hint_state_id IN (3,4,5,6,7)
                               AND bhl.hint_type_id=2
                               AND bhj.jurisdiction_accepted = 1
                               AND b2ha.assessment_accepted = 1
                               AND bhu.url_accepted=1
                               AND bht.language_id=1;")
  
  deliver.output=unique(ricardo.data)
  
  Encoding(deliver.output$hint.title)="UTF-8"
  Encoding(deliver.output$hint.description)="UTF-8"
  deliver.output <<- deliver.output
}