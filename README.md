# notcursesada
This is an Ada binding for the notcurses library. [notcurses](https://notcurses.com/) is a text user interface library for modern terminals.

# Project status
notcursesada is a work in progress and should not be used for production code. The API may change without notice. Many features of notcurses are not yet implemented.

# Usage
Add notcursesada to your project with [Alire](https://alire.ada.dev/)

    alr with notcursesada --use=https://github.com/JeremyGrosser/notcursesada

See [tests.adb](tests/src/tests.adb) for examples.

# Running tests

    cd tests
    alr run

# License
Copyright 2021-2022 Jeremy Grosser <jeremy@synack.me>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
