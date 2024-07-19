# Shert
A library for reading office open XML spreadsheetML spreadsheets (aka ms office xlsx).
The name of the project is evocative of my feelings towards this file format and Microsoft.

## Instructions
### Building
```sh
erlc *.erl
```

### Usage
See the exports of `shert.erl`.

### Requirements
- 7zip; the command line tool must be in your system path.
  
The Erlang zip library doesn't support zip64 and some other
things, so I decided on a wrapper around 7zip instead.
I would've used unzip/zipinfo, but those aren't easily
available for windows users. :/

## Considerations
### Storage
Shert loads spreadsheets into an ETS table. you can either pass your own table to it
or you can let it create a new one. Tables should have keypos 1 and be of set type.

### Formatting
I haven't completed cellXfs formatting support (number formats) for Shert yet, so style
sheets will be completely ignored. You can currently only view the data as contained
in each sheet.
