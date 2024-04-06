# Use Ubuntu 22.04 as base image
FROM ubuntu:22.04

# Atualizar o repositório e instalar pacotes essenciais
RUN apt-get update && \
    apt-get install -y \
    openssh-server \
    openjdk-8-jdk \
    sudo \
    && rm -rf /var/lib/apt/lists/*

# Configurar o SSH
RUN mkdir /var/run/sshd
RUN echo 'root:root' | chpasswd
RUN sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -i 's/#PasswordAuthentication yes/PasswordAuthentication yes/' /etc/ssh/sshd_config
RUN sed -i 's/#ListenAddress 0.0.0.0/ListenAddress 0.0.0.0/' /etc/ssh/sshd_config

# Add Hadoop user
RUN useradd -m glauco
RUN echo "glauco ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN echo 'glauco:glauco' | chpasswd

# Expor a porta 22 para o SSH
EXPOSE 22

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
RUN exec bash

RUN mkdir -p /home/glauco/.ssh && \
    ssh-keygen -t rsa -P '' -f /home/glauco/.ssh/id_rsa && \
    cat /home/glauco/.ssh/id_rsa.pub >> /home/glauco/.ssh/authorized_keys && \
    chmod 0640 /home/glauco/.ssh/authorized_keys && \
    chown -R glauco:glauco /home/glauco/.ssh && \
    chown -R glauco:glauco /opt/hadoop

# Definir o comando padrão para iniciar o SSH
CMD ["/usr/sbin/sshd", "-D"]
