# CEDS810

docker compose up -d

ssh glauco@localhost

senha glauco

hadoop version

hdfs namenode -format

start-all-custom.sh

jps

hdfs dfs -mkdir -p /tmp
hdfs dfs -chmod -R 777 /tmp
hdfs dfs -mkdir -p /logs
hdfs dfs -chmod -R 777 /logs
hdfs dfs -mkdir -p /user
hdfs dfs -chmod -R 777 /user
hdfs dfs -mkdir -p /var
hdfs dfs -chmod -R 777 /var

yarn jar /opt/hadoop/share/hadoop/mapreduce/hadoop-mapreduce-examples-3.3.6.jar pi 2 4

jupyter-notebook &

http://127.0.0.1:8888/tree?token=token_fixo

http://localhost:8080

pyspark --master="spark://127.0.0.1:7077"

http://localhost:4040

stop-all-custom.sh