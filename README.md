[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](./LICENSE)

---

# MetaFlux

MetaFlux is a web application for metabolic flux modelling. This project is a work in progress.

## Install pre-requisites

You'll need to install the following pre-requisites in order to build MetaFlux:

* [.NET Core SDK](https://www.microsoft.com/net/download) 5.0 or higher
* [Node LTS](https://nodejs.org/en/download/)

## Starting the application

Before you run the project **for the first time only** you must install dotnet "local tools" with this command:

```bash
dotnet tool restore
```

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet run
```

Then open `http://localhost:8080` in your browser.

The build project in root directory contains a couple of different build targets. You can specify them after `--` (target name is case-insensitive).

To run concurrently server and client tests in watch mode (you can run this command in parallel to the previous one in new terminal):

```bash
dotnet run -- RunTests
```

Client tests are available under `http://localhost:8081` in your browser and server tests are running in watch mode in console.

Finally, there are `Bundle` and `Azure` targets that you can use to package MateFlux and deploy to Azure, respectively:

```bash
dotnet run -- Bundle
dotnet run -- Azure
```
