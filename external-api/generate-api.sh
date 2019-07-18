cd "$(dirname "$0")"/../identityrpc/compiler/IdentityRPC.Compiler.Cli

dotnet run typescript ../../../external-api/js-api.idrpc ../../../external-api/node-api/src/api.gen.ts
dotnet run scala ../../../external-api/js-api.idrpc ../../../argon-build/src/main/scala/dev/argon/build/testrunner/node/api.gen.scala

