## Release
### Synopsis
We created ```release``` to simplify, standardize and to speed up creation of
branches, tags, commit-messages and ```pom.xml``` modifications.
It also checks your ```pom.xml``` for problems and suggests hints to fix them. e.g.
* no -SNAPSHOTS in releases
* matching release major versions of ishop-core
* create branches
* modify GAV (GroupId ArtifactId Version)
* ...

### Usage
This projects contains an executable called ```release```. To use it, checkout this
project and execute it in your project.

e.g.
```
cd the/project
${HOME}/git/release/release
```
This assumes you checked out "release" in ```${HOME}/git```. It is also possible
to add ```release``` to your ```PATH``` and call it without an absolute path.
```
cd the/project
release
```

If you want to learn more options of ```release``` try ```release --help```.

## Features
* pom.xml validation
  * SNAPSHOT checks

* release from:
  * master
  * detached HEAD
