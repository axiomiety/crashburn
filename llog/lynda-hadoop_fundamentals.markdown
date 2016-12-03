---
layout: default
title: llog - lynda, hadoop fundamentals
category: pages
---

Some quick notes on the Lynda course of the same name. Best seen raw as it isn't yet formatted.

{% highlight bash %}

Hadoop = file system (HDFS) with a processing library
HDFS - based on GFS

HBase - NoSQL db (wide columnstore). E.g. CustomerID -> {attr1: val1, attr2: val2, ... } - width of column varies depending on the amount of info (vs RDBMS)

CAP - Consistency, Availability, Partitioning
Consistency = transactions
Availability = up-time (copies of the data)
Partitioning = scalability (split the set of data across your infrastructure)

CAP theory says you can only have 2 of the 3

Where does Hadoop fit?
Commodity hardware
Scalability - partitioning (by default, 3 copies of the data)
Flexibility - can scale almost infinitely

Not really designed for transactional data (e.g. making money!)
Behavioural data - processed as a group/batch, very well suited
Often need to add a NoSQL system with Hadoop (where the Hadoop output often ends up)
Hadoop is not a replacement for relational database systems, but a supplement

2 components - HDFS (open-source data storage) + processing API (MapReduce)
Often includes other components like Pig, Hive, ...
Different distributions available
* Apache Hadoop
Commercial distribution
* Cloudera
* Hortonworks
* MapR
Hadoop clusters on the cloud (e.g. MapR on AWS)
* AWS
* Windows Azure HDInsight

Hortonworks - former Yahoo employee
eBay, American Airlines (behavioural data), IBM, Federal Reserve Board, ...

# Hadoop vs HBase
64MB/128MB chunks for HBase
HBase - schema on read
Hive - query language on top of HBase

# Hadoop file systems
* HDFS (fully distributed vs pseudo-distributed)
* Regular file system (standalone)
* Cloud file system (AWS:S3, Azure:BLOB)

Files vs JVMs
* Single node -> local + single JVM
* Pseudo-distributed -> distributed, more JVMs (which don't share state)

Hadoop Ecosystem
* MapReduce2 is built on top of MapReduce1
* Hive - used to query HBase
* Pig - ETL processes
* Mahout - machine learning/predictive
* Oozie/Zookeeper - coordination
* Sqoop - data exchange, like with SQLServer
* Flume - log collector
* Ambari - managing Hadoop clusters

Cloudera, Hortonworks - online sandbox
MapR - uses NoSQL

# Cloudera Live
Hive (HQL)/Impala/Pig - high level
Job Designer - lower level

# Cloud-based distributions
AWS: Elastic MapReduce
Microsoft: HDInsight

### Ch4 skipped

Mapper & reducer
Mapper
* for each word in file, emit [(word, 1)]
Reducer
* for each word in words, add {word, count}

Note the 'shuffle' stage which groups keys together

Default job will require you to implement the mapping and the reduce logic
org.apache.hadoop.mapred -> v1
org.apache.hadoop.madpreduce -> v2

Java: JobConf, pass in the classes (setOutputKeyClass, setOutputValueClass, ...)
Note the use of a Combiner to reduce network traffic

{% endhighlight %}
