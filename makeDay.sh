#!/bin/bash

# echo "$( dirname "${BASH_SOURCE[0]}" )"

if [ $# -eq 1  ]
then
    PROJECT_NAME="day$1"
    dotnet new sln -o $PROJECT_NAME
    cd $PROJECT_NAME
    dotnet new classLib -lang F# -o src/Core
    dotnet new xunit -lang F# -o tests/Core.Tests
    dotnet add tests/Core.Tests package FsUnit

    dotnet add tests/Core.Tests/Core.Tests.fsproj reference src/Core/Core.fsproj

    dotnet sln add src/Core/Core.fsproj
    dotnet sln add tests/Core.Tests/Core.Tests.fsproj
else
    echo "You should give the day of the event :)"
fi