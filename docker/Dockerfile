# Use Ubuntu 22.04 as base image
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# Atualizar o repositório e instalar pacotes essenciais
RUN apt-get update && \
    apt-get install -y \
    openssh-server \
    openjdk-8-jdk \
    python3 \
    python3-pip \
    ipython3 \
    scala \
    sudo \
    nano \
    && rm -rf /var/lib/apt/lists/*

# Configurar o SSH
RUN mkdir /var/run/sshd
RUN echo 'root:root' | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -i 's/#PasswordAuthentication yes/PasswordAuthentication yes/' /etc/ssh/sshd_config
RUN sed -i 's/#ListenAddress 0.0.0.0/ListenAddress 0.0.0.0/' /etc/ssh/sshd_config
# RUN sed -i 's/#Port 22/Port 2222/' /etc/ssh/sshd_config

# Add Hadoop user
RUN useradd -m glauco
RUN echo "glauco ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN echo 'glauco:glauco' | chpasswd

# Expor a porta 2222 para o SSH
# EXPOSE 2222
EXPOSE 22
# Criando pasta para downloads
RUN mkdir /home/glauco/installation

# Configurando o Hadoop
RUN wget -O /home/glauco/installation/hadoop-3.3.6.tar.gz https://dlcdn.apache.org/hadoop/common/hadoop-3.3.6/hadoop-3.3.6.tar.gz
RUN tar -xzvf /home/glauco/installation/hadoop-3.3.6.tar.gz -C /opt
RUN mv /opt/hadoop-3.3.6 /opt/hadoop
RUN chmod -R 777 /opt/hadoop
RUN mkdir -p /opt/hadoop/data/namenode
RUN mkdir -p /opt/hadoop/data/datanode
RUN mkdir /opt/tmp
RUN mkdir /opt/logs
RUN echo '' >> /opt/hadoop/etc/hadoop/hadoop-env.sh && \
    echo 'export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64' >> /opt/hadoop/etc/hadoop/hadoop-env.sh && \
    echo 'export HADOOP_CONF_DIR=/opt/hadoop/etc/hadoop' >> /opt/hadoop/etc/hadoop/hadoop-env.sh && \
    echo 'export HADOOP_HOME=/opt/hadoop' >> /opt/hadoop/etc/hadoop/hadoop-env.sh && \
    echo 'export HADOOP_PID_DIR=/opt/hadoop/tmp' >> /opt/hadoop/etc/hadoop/hadoop-env.sh && \
    echo 'export HADOOP_SECURE_DN_PID_DIR=/opt/hadoop/tmp' >> /opt/hadoop/etc/hadoop/hadoop-env.sh && \
    echo 'export HADOOP_IDENT_STRING=$USER' >> /opt/hadoop/etc/hadoop/hadoop-env.sh
COPY ./configs/ /opt/hadoop/etc/hadoop/
RUN echo '' >> /home/glauco/.bashrc && \
    echo 'export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64' >> /home/glauco/.bashrc && \
    echo 'export PATH=$PATH:$JAVA_HOME/bin' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_HOME=/opt/hadoop' >> /home/glauco/.bashrc && \
    echo 'export PATH=$PATH:$HADOOP_HOME/bin' >> /home/glauco/.bashrc && \
    echo 'export PATH=$PATH:$HADOOP_HOME/bin:$HADOOP_HOME/sbin:$HADOOP_HOME/etc/hadoop' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_LOG_DIR=$HADOOP_HOME/logs' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_PID_DIR=$HADOOP_HOME/tmp' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_CONF_DIR=$HADOOP_HOME/etc/hadoop' >> /home/glauco/.bashrc && \
    echo 'export CLASSPATH=$JAVA_HOME/lib/tools.jar:$HADOOP_HOME/lib/native/*:$JAVA_HOME/lib:$HADOOP_HOME/share/hadoop/common/*:$HADOOP_HOME/share/hadoop/common/lib/*:$HADOOP_HOME/share/hadoop/hdfs/*:$HADOOP_HOME/share/hadoop/hdfs/lib/*:$HADOOP_HOME/share/hadoop/tools/lib/*:$HADOOP_HOME/share/hadoop/mapreduce/*:$HADOOP_HOME/share/hadoop/yarn/*:$HADOOP_HOME/share/hadoop/yarn/lib/*' >> /home/glauco/.bashrc && \
    echo 'export JAVA_LIBRARY_PATH=$JAVA_HOME/lib:$HADOOP_HOME/lib/native/*:$HADOOP_HOME/share/hadoop/common/lib/*' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_CLASSPATH=$HADOOP_HOME/share/hadoop/common/lib/*.jar' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_HOME_WARN_SUPPRESS=1' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_ROOT_LOGGER=WARN,DRFA' >> /home/glauco/.bashrc

# Criando script personalizado de start e stop all
RUN sed -i '/sleep 10/d' /opt/hadoop/sbin/start-all.sh
RUN sed -i '/sleep 10/d' /opt/hadoop/sbin/stop-all.sh

RUN touch /opt/hadoop/sbin/start-all-custom.sh && \
    echo '#!/bin/bash' >> /opt/hadoop/sbin/start-all-custom.sh && \
    echo '/opt/hadoop/sbin/start-all.sh' >> /opt/hadoop/sbin/start-all-custom.sh && \
    echo '/opt/hadoop/spark/sbin/start-master.sh' >> /opt/hadoop/sbin/start-all-custom.sh && \
    echo '/opt/hadoop/spark/sbin/start-workers.sh' >> /opt/hadoop/sbin/start-all-custom.sh && \
    chmod +x /opt/hadoop/sbin/start-all-custom.sh

RUN touch /opt/hadoop/sbin/stop-all-custom.sh && \
    echo '#!/bin/bash' >> /opt/hadoop/sbin/stop-all-custom.sh && \
    echo '/opt/hadoop/sbin/stop-all.sh' >> /opt/hadoop/sbin/stop-all-custom.sh && \
    echo '/opt/hadoop/spark/sbin/stop-master.sh' >> /opt/hadoop/sbin/stop-all-custom.sh && \
    echo '/opt/hadoop/spark/sbin/stop-workers.sh' >> /opt/hadoop/sbin/stop-all-custom.sh && \
    chmod +x /opt/hadoop/sbin/stop-all-custom.sh
RUN exec bash

# Permissao de ssh para o usuario hadoop
RUN mkdir -p /home/glauco/.ssh && \
    ssh-keygen -t rsa -P '' -f /home/glauco/.ssh/id_rsa && \
    cat /home/glauco/.ssh/id_rsa.pub >> /home/glauco/.ssh/authorized_keys && \
    chmod 0640 /home/glauco/.ssh/authorized_keys && \
    chown -R glauco:glauco /home/glauco/.ssh && \
    chown -R glauco:glauco /opt/hadoop


# Preparando o PYspark
RUN pip3 install jupyter py4j
RUN echo '' >> /home/glauco/.bashrc && \
    echo 'export PATH=$PATH:/home/glauco/.local/bin' >> /home/glauco/.bashrc && \
    echo 'export JRE_HOME=$JAVA_HOME/jre' >> /home/glauco/.bashrc && \
    echo 'alias jupyter-notebook="/usr/local/bin/jupyter-notebook --no-browser --ip=0.0.0.0 --NotebookApp.token=token_fixo"' >> /home/glauco/.bashrc
RUN wget -O /home/glauco/installation/spark-3.5.1-bin-without-hadoop.tgz https://dlcdn.apache.org/spark/spark-3.5.1/spark-3.5.1-bin-without-hadoop.tgz
RUN tar -xzvf /home/glauco/installation/spark-3.5.1-bin-without-hadoop.tgz -C /opt/hadoop
RUN mv /opt/hadoop/spark-3.5.1-bin-without-hadoop /opt/hadoop/spark
RUN chmod -R 777 /opt/hadoop/spark
RUN echo '' >> /home/glauco/.bashrc && \
    echo 'export SPARK_HOME=/opt/hadoop/spark' >> /home/glauco/.bashrc && \
    echo 'export SPARK_DIST_CLASSPATH=$HADOOP_HOME/etc/hadoop/*:$HADOOP_HOME/share/hadoop/common/lib/*:$HADOOP_HOME/share/hadoop/common/*:$HADOOP_HOME/share/hadoop/hdfs/*:$HADOOP_HOME/share/hadoop/hdfs/lib/*:$HADOOP_HOME/share/hadoop/hdfs/*:$HADOOP_HOME/share/hadoop/yarn/lib/*:$HADOOP_HOME/share/hadoop/yarn/*:$HADOOP_HOME/share/hadoop/mapreduce/lib/*:$HADOOP_HOME/share/hadoop/mapreduce/*:$HADOOP_HOME/share/hadoop/tools/lib/*' >> /home/glauco/.bashrc && \
    echo 'export PYTHONPATH=$SPARK_HOME/python:$PYTHONPATH' >> /home/glauco/.bashrc && \
    echo 'export PYSPARK_DRIVER_PYTHON="jupyter"' >> /home/glauco/.bashrc && \
    echo 'export PYSPARK_DRIVER_PYTHON_OPTS="notebook"' >> /home/glauco/.bashrc && \
    echo 'export PYSPARK_PYTHON=python3' >> /home/glauco/.bashrc && \
    echo 'export PATH=$PATH:$SPARK_HOME:$JAVA_HOME/bin:$JRE_HOME/bin:$SPARK_HOME/bin:$SPARK_HOME/sbin' >> /home/glauco/.bashrc && \
    echo 'export CLASSPATH=$CLASSPATH:$SPARK_HOME/jars' >> /home/glauco/.bashrc && \
    echo 'export HADOOP_CLASSPATH=$CLASSPATH' >> /home/glauco/.bashrc && \
    echo 'export JAVA_LIBRARY_PATH=$CLASSPATH' >> /home/glauco/.bashrc
RUN mkdir /home/glauco/.jupyter
RUN chmod -R 777 /home/glauco/.jupyter

WORKDIR /opt/hadoop/spark/conf
RUN mv spark-env.sh.template spark-env.sh
RUN mv workers.template workers
RUN mv spark-defaults.conf.template spark-defaults.conf

RUN echo '' >> /opt/hadoop/spark/conf/spark-defaults.conf && \
    echo 'spark.yarn.preserve.staging.files    true' >> /opt/hadoop/spark/conf/spark-defaults.conf && \
    echo 'spark.authenticate    true' >> /opt/hadoop/spark/conf/spark-defaults.conf && \
    echo 'spark.authenticate.secret    token_fixo' >> /opt/hadoop/spark/conf/spark-defaults.conf && \
    echo 'spark.master    spark://localhost:7077' >> /opt/hadoop/spark/conf/spark-defaults.conf
    

RUN echo '' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export HADOOP_HOME=/opt/hadoop' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export SPARK_DIST_CLASSPATH=$(/opt/hadoop/bin/hadoop classpath)' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export HADOOP_CONF_DIR=/opt/hadoop/etc/hadoop' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export YARN_CONF_DIR=/opt/hadoop/etc/hadoop' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export SPARK_MASTER_HOST=0.0.0.0' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export SPARK_HOME=/opt/hadoop/spark' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export SPARK_CONF_DIR=${SPARK_HOME}/conf' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export SPARK_LOG_DIR=${SPARK_HOME}/logs' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export PATH=$PATH:$SPARK_HOME/bin:$SPARK_HOME/sbin' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export CLASSPATH=$CLASSPATH:/opt/hadoop/spark/jars' >> /opt/hadoop/spark/conf/spark-env.sh && \
    echo 'export HADOOP_CLASSPATH=$CLASSPATH' >> /opt/hadoop/spark/conf/spark-env.sh

RUN ln -s /opt/hadoop/etc/hadoop/core-site.xml /opt/hadoop/spark/conf/core-site.xml

WORKDIR /opt/hadoop/spark/jars
RUN mv orc-core-1.9.2-shaded-protobuf.jar orc-core-1.9.2-shaded-protobuf.jarold
RUN mv orc-mapreduce-1.9.2-shaded-protobuf.jar orc-mapreduce-1.9.2-shaded-protobuf.jarold
RUN mv orc-shims-1.9.2.jar orc-shims-1.9.2.jarold

# Shell padrao ao logar
RUN chsh -s /bin/bash glauco

# Definir o comando padrão para iniciar o SSH
CMD ["/usr/sbin/sshd", "-D"]
