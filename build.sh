echo "> stack build"
stack build
echo "> copying executable to root dir"
rm curryfish-exe
cp .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/curryfish-exe/curryfish-exe ./