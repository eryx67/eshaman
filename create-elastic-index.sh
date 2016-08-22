#!/bin/bash

ESHOST="localhost:9200"

curl -XPUT "http://${ESHOST}/_template/shaman" -d '{
    "template": "shaman-*",
    "settings": {
        "number_of_shards": 1,
        "number_of_replicas": 0,
        "index.refresh_interval": "5s"
    },
    "mappings": {
        "ping": {
            "properties": {
                "@timestamp": {
                    "type": "date",
                    "format": "yyyy-MM-dd HH:mm:ss||date_time"

                },
                "domain": {
                    "type": "string",
                    "index" : "not_analyzed"

                },
                "group": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "name": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "last": {
                    "type": "float"
                },
                "mean": {
                    "type": "float"
                },
                "median": {
                    "type": "float"
                },
                "min": {
                    "type": "float"
                },
                "max": {
                    "type": "float"
                }

            }
        },

        "http_ping": {
            "properties": {
                "@timestamp": {
                    "type": "date",
                    "format": "yyyy-MM-dd HH:mm:ss||date_time"

                },
                "domain": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "group": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "name": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "last": {
                    "type": "float"
                },
                "mean": {
                    "type": "float"
                },
                "median": {
                    "type": "float"
                },
                "min": {
                    "type": "float"
                },
                "max": {
                    "type": "float"
                }

            }
        },

        "cpu": {
            "properties": {
                "@timestamp": {
                    "type": "date",
                    "format": "yyyy-MM-dd HH:mm:ss||date_time"

                },
                "domain": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "group": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "name": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "nprocs": {
                    "type": "float"
                },
                "avg1": {
                    "type": "float"
                },
                "avg5": {
                    "type": "float"
                },
                "avg15": {
                    "type": "float"
                },
                "kernel": {
                    "type": "float"
                },
                "user": {
                    "type": "float"
                },
                "idle": {
                    "type": "float"
                },
                "wait": {
                    "type": "float"
                }
            }
        },

        "memory": {
            "properties": {
                "@timestamp": {
                    "type": "date",
                    "format": "yyyy-MM-dd HH:mm:ss||date_time"

                },
                "domain": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "group": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "name": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "total": {
                    "type": "float"
                },
                "used": {
                    "type": "float"
                },
                "capacity": {
                    "type": "float"
                },
                "buffered": {
                    "type": "float"
                },
                "cached": {
                    "type": "float"
                },
                "mean": {
                    "type": "float"
                },
                "max": {
                    "type": "float"
                },
                "min": {
                    "type": "float"
                }
            }
        },

        "disk": {
            "properties": {
                "@timestamp": {
                    "type": "date",
                    "format": "yyyy-MM-dd HH:mm:ss||date_time"

                },
                "domain": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "group": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "name": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "file_system": {
                    "type": "string",
                    "index" : "not_analyzed"
                },
                "value": {
                    "type": "float"
                }
            }
        }
    }
}'
