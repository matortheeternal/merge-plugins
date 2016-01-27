## Description
Merge Plugins Standalone (or just Merge Plugins), is a tool for combining mods from Bethesda games which have .ESP/.ESM files.  It uses the xEdit codebase as an API, and can be used with Skyrim, Oblivion, Fallout New Vegas, and Fallout 3.  Merge Plugins Standalone is the successor of my Merge Plugins xEdit Script.  Feel free to check out the [screenshot gallery](http://imgur.com/a/P3Df2) to get an idea of the look and feel of Merge Plugins.

You can also check out GamerPoet's [video tutorial](https://www.youtube.com/watch?v=0S6cpCwTezE).

## Compiling
This project can be compiled with RAD Studio Delphi XE, or newer.  You can download compiled builds from the [releases section](http://github.com/matortheeternal/merge-plugins/releases) here on GitHub.

## Usage
The exectutable will automatically detect load orders for Skyrim, Oblivion, Fallout New Vegas, and Fallout 3.  This exectuable should be run through Mod Organizer if you want it to use mods installed with Mod Organizer.  There are multiple integration features for working with Mod Organizer, so it is the recommended load order tool for use with this utility.

## Setup
1. Unpack Merge Plugins somewhere.  E.g. C:\Program Files\Merge Plugins
2. Start Merge Plugins.
3. If you don't see a profile for your game, right-click in the box and choose "New profile", then specify a profile name, game mode, and game path.  You can browse for your game path by clicking on the browse icon.
4. If your profile is valid, its background color will turn a shade of light green.  You can then select the profile by clicking the game icon (or any other part of the panel).
5. Once you've selected a profile, click "OK".

## Configuration
When you first start Merge Plugins you should do some initial configuration.
To start, click on the gear icon near the top of the window, this will open the
Options Form.  Once there, you'll want to: 

### (1) configure integrations
Go to the Integrations Tab.  If you're using Mod Organizer, check the "I'm using Mod Organizer" checkbox.  Then click Detect Integrations.  If any integrations aren't found, you'll want to install them or browse to find them on your system.  Make sure you verify the paths found by Merge Plugins, especially for Mod Organizer if you have multiple installations.

### (2) set your asset destination directory
Go to the Merging Tab and click the browse icon by the text box.  The folder you choose here will be where merges are placed.  If you're using Mod Organizer this should be your Mod Organizer mods folder.

### (3) set merging options
Go to the Merging Tab and check the boxes corresponding to the options you want to use.  You'll want to use "Extract BSAs" or "Build Merged BSA" if you aren't using Mod Organizer.  You can leave everything else as the defaults.

### (4) register a username
Go to the General Tab and enter a username in the username text box.  Click "Check" to verify the username is available.  Once you have a username that's available, click "Register".  This username is put on reports submitted to the backend.  A username is required if you want to submit reports on plugins you've merged.

## Credits
* **xEdit**: used for interfacing with Bethesda plugin files.  xEdit was designed by zilav, hlp, Sharlikran and ElminsterAU
* **superobject**: used for handling json.
* **Abbrevia**: used for handling zip archives.
* **ZEOSDBO**: used for interfacing with MySQL database.

## License
Merge Plugins is licensed under the MPL.  Please see license.txt for more details.
