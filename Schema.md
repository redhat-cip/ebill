# Views and design of eBill couchDB db

## views

### find_by_resource_metric_and_date

```javascript
function (doc) {
 if (doc.project_id && doc.resource_id && doc.metric && doc.date) {
 emit([doc.project_id, doc.resource_id, doc.metric, doc.date], doc);
}
}
```

### find_by_resource_and_date

```javascript
function (doc) {
 if (doc.project_id && doc.resource_id && doc.date) {
 emit([doc.project_id, doc.resource_id, doc.date], doc);
}
}
```

### find_by_metric_and_date

```javascript
function (doc) {
 if (doc.project_id && doc.metric && doc.date) {
 emit([doc.project_id, doc.metric, doc.date], doc);
}
}
```

### find_by_date

```javascript
function (doc) {
 if (doc.project_id && doc.date) {
 emit([doc.project_id, doc.date], doc);
}
}
```

### find_by_resource

```javascript
function (doc) {
 if (doc.project_id && doc.resource_id) {
 emit([doc.project_id, doc.resource_id], doc);
}
}
```

### find_by_metric

```javascript
function (doc) {
 if (doc.project_id && doc.metric) {
 emit([doc.project_id, doc.metric], doc);
}
}
```

### find_by_ID

```javascript
function (doc) {
 if (doc.project_id) {
 emit(doc.project_id, doc);
}
}
```

### find_by_resource_and_metric

```javascript
function (doc) {
 if (doc.project_id && doc.resource_id && doc.metric) {
 emit([doc.project_id, doc.resource_id, doc.metric], doc);
}
}
```

### metric_list_for_id

```javascript
function (doc) {
 if (doc.project_id && doc.metric) {
 emit([doc.project_id, doc.metric], null);
}
}
```

### metric_list

```javascript
function (doc) {
 if (doc.metric) {
 emit(doc.metric, null);
}
}
```
