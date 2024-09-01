
# CouchDB HTTP API Cheatsheet

## 1. Basic Commands

- **Check CouchDB Version:**
  ```bash
  curl http://127.0.0.1:5984/
  ```

- **List All Databases:**
  ```bash
  curl -X GET http://127.0.0.1:5984/_all_dbs
  ```

## 2. Database Operations

- **Create a Database:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/<database_name>
  ```

- **Delete a Database:**
  ```bash
  curl -X DELETE http://127.0.0.1:5984/<database_name>
  ```

- **Get Database Information:**
  ```bash
  curl -X GET http://127.0.0.1:5984/<database_name>
  ```

## 3. Document Operations

- **Create or Update a Document:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/<database_name>/<document_id> \
       -H "Content-Type: application/json" \
       -d '{"name": "Alice", "age": 30}'
  ```

- **Auto-generate Document ID:**
  ```bash
  curl -X POST http://127.0.0.1:5984/<database_name> \
       -H "Content-Type: application/json" \
       -d '{"name": "Alice", "age": 30}'
  ```

- **Retrieve a Document:**
  ```bash
  curl -X GET http://127.0.0.1:5984/<database_name>/<document_id>
  ```

- **Update a Document:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/<database_name>/<document_id> \
       -H "Content-Type: application/json" \
       -d '{"_id": "<document_id>", "_rev": "<revision_id>", "name": "Alice", "age": 31}'
  ```

- **Delete a Document:**
  ```bash
  curl -X DELETE http://127.0.0.1:5984/<database_name>/<document_id>?rev=<revision_id>
  ```

- **List All Documents in a Database:**
  ```bash
  curl -X GET http://127.0.0.1:5984/<database_name>/_all_docs
  ```

- **Retrieve Documents with Content:**
  ```bash
  curl -X GET http://127.0.0.1:5984/<database_name>/_all_docs?include_docs=true
  ```

## 4. Bulk Operations

- **Bulk Insert or Update Documents:**
  ```bash
  curl -X POST http://127.0.0.1:5984/<database_name>/_bulk_docs \
       -H "Content-Type: application/json" \
       -d '{"docs": [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]}'
  ```

## 5. Design Documents and Views

- **Create a Design Document:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/<database_name>/_design/<design_doc_name> \
       -H "Content-Type: application/json" \
       -d '{
             "_id": "_design/<design_doc_name>",
             "views": {
               "by_name": {
                 "map": "function(doc) { if (doc.name) emit(doc.name, doc); }"
               }
             }
           }'
  ```

- **Query a View:**
  ```bash
  curl -X GET http://127.0.0.1:5984/<database_name>/_design/<design_doc_name>/_view/by_name
  ```

## 6. Replication

- **Start Replication (source to target):**
  ```bash
  curl -X POST http://127.0.0.1:5984/_replicate \
       -H "Content-Type: application/json" \
       -d '{"source": "<source_db>", "target": "<target_db>"}'
  ```

- **Continuous Replication:**
  ```bash
  curl -X POST http://127.0.0.1:5984/_replicate \
       -H "Content-Type: application/json" \
       -d '{"source": "<source_db>", "target": "<target_db>", "continuous": true}'
  ```

## 7. User Management

- **Create an Admin User:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/_node/_local/_config/admins/<username> \
       -d '"<password>"'
  ```

- **Add a Database Reader User:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/_users/org.couchdb.user:<username> \
       -H "Content-Type: application/json" \
       -d '{"name": "<username>", "password": "<password>", "roles": [], "type": "user"}'
  ```

## 8. Security

- **Set Database Security:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/<database_name>/_security \
       -H "Content-Type: application/json" \
       -d '{"admins": {"names": ["admin"], "roles": ["admins"]}, "members": {"names": [], "roles": ["users"]}}'
  ```

## 9. Server Configuration

- **Get Server Configuration:**
  ```bash
  curl -X GET http://127.0.0.1:5984/_config
  ```

- **Set a Configuration Value:**
  ```bash
  curl -X PUT http://127.0.0.1:5984/_config/<section>/<key> \
       -d '"<value>"'
  ```

## 10. Database Compaction

- **Compact a Database:**
  ```bash
  curl -X POST http://127.0.0.1:5984/<database_name>/_compact
  ```

- **Compact Views:**
  ```bash
  curl -X POST http://127.0.0.1:5984/<database_name>/_compact/<design_doc_name>
  ```

### Conclusion

This cheatsheet provides a quick reference to commonly used CouchDB commands via its REST API. CouchDB’s RESTful API allows seamless interaction with databases and documents using HTTP methods. Make sure to replace placeholders (like `<database_name>`, `<document_id>`, `<design_doc_name>`, etc.) with your specific values.
