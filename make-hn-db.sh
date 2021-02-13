#!/bin/sh

sqlite3 hn.db -line "CREATE TABLE Item (Id INT, Title NVARCHAR, Url NVARCHAR)"
