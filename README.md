# CEDS810

docker compose up -d

ssh glauco@localhost -p 2222

senha glauco

ssh localhost

exec bash

hadoop version

hdfs namenode –format

start-dfs.sh

jps

hdfs dfs -mkdir -p /tmp
hdfs dfs -chmod -R 777 /tmp
hdfs dfs -mkdir -p /logs
hdfs dfs -chmod -R 777 /logs
hdfs dfs -mkdir -p /user
hdfs dfs -chmod -R 777 /user
hdfs dfs -mkdir -p /var
hdfs dfs -chmod -R 777 /var

start-yarn.sh

jps

yarn jar /opt/hadoop/share/hadoop/mapreduce/hadoop-mapreduce-examples-3.3.6.jar pi 2 4

stop-yarn.sh

stop-dfs.sh