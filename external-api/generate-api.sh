cd "$(dirname "$0")"/../gcrpc/compiler/

dotnet run typescript ../../external-api/js-api.gcrpc ../../external-api/node-api/src/api.gen.ts
dotnet run scala ../../external-api/js-api.gcrpc ../../argon-build/src/main/scala/com/mi3software/argon/build/testrunner/node/api.gen.scala

