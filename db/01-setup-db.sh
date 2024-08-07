#!/bin/bash
set -e

source ../.secrets/db-admin.env

DB_URL="postgresql://$DB_USER:$DB_PASSWORD@$DB_HOST:$DB_PORT"

# Set level_up_shiny user password
# psql "$DB_URL/defaultdb" -c "ALTER USER level_up_shiny WITH PASSWORD 'xxxxxxxxxxxxx';"

databases=("college_scorecard" "college_scorecard_staging")

for db in "${databases[@]}"; do
    echo "Creating $db"
    psql "$DB_URL/$DB_DATABASE" -c "DROP DATABASE IF EXISTS $db;"
    psql "$DB_URL/$DB_DATABASE" -c "CREATE DATABASE $db;"

    echo "Setting up $db"
    (cd .. && Rscript db/02-setup-data.R "$db")

    echo "Granting permissions to level_up_shiny on $db"
    psql "$DB_URL/$db" -c 'GRANT SELECT ON TABLE school to level_up_shiny;'
    psql "$DB_URL/$db" -c 'GRANT SELECT ON TABLE scorecard to level_up_shiny;'
done
