#!/usr/bin/env sh

sed -i 's/-lnetcdff -lnetcdf/-lnetcdff -lnetcdf -liomp5 -lpthread/' configure.wps

