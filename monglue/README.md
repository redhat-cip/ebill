# Monglue

## monglue-register

Intégration entre le monitoring et le billing. Lorsque qu'une VM démarre, elle notifie grâce à ses agents le service de monitoring
afin de commencer la remonté des métriques. A ce moment, le monitoring notifie aussi ce service REST afin d'enregistrer les VM
à facturer.

On prends pour exemple a7d9cbce-359d-4311-b98f-7fbb53c71464 comme vmid. (VAMP)

### Ajout d'une VM pour la facturation (via un GET ou un POST)

$ curl -i -X GET http://10.197.180.216:8070/bill/a7d9cbce-359d-4311-b98f-7fbb53c71464

Ou

$ curl -i -X POST -H "Content-Type: application/json" http://10.197.180.216:8070/ -d "{ \
 \"vmid\": \"a7d9cbce-359d-4311-b98f-7fbb53c71464\" \
}"

### Retrait d'une VM lors de sa suppression (ou plus de billing)

$ curl -i -X DELETE http://10.197.180.216:8070/bill/a7d9cbce-359d-4311-b98f-7fbb53c71464

### Liste des VM actuellement billés

$ curl -i -X GET http://10.197.180.216:8070/list

L'application est basé sur un redis (pour la liste des vm à biller), et sinatra (ruby).

## monglue-control

Une fois que les id de vm sont enregistrés dans la base redis, on demande les
métriques correspondantes à ces VM au service de monitoring. Celui-ci lui
réponds en JSON, monglue-control fait donc ensuite une requête à ebill storage
avec les métriques à enregistrer et faire un historique sur ces données pour
une facturation ultérieure.

`while true;do ruby monglue-control.rb;sleep 30;done`
