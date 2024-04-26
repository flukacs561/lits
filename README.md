# LiTS

LiTS is a commandline tool for tagging and based on these tags querying a collection of digital books.

# Commands
- `init`: initialises lits in the current directory with an empty database (`.lits.json`) file. 
  - no arguments
- `list`: lists all entries in the database sorted by the last name of the first author
  - no arguments
- `add`: adds entry for file to the database. Prompts for author, title and tags
  - file
- `delete`: delete an entry from the database. If a corresponding file is present in the directory, delete that too.
- `check`: checks if all files in the current folder (except for `.lits.json`) have an entry in the database, and conversely that all entries in the database correspond to an actual file in the directory. (Itt az outputformátumot kellene megtervezni.)
  - no arguments
- `clean-up`: delete all entries from the database that do not have a corresponding file in the directory.
  - no arguments
- `add-tag`: add specified tags from to entry that corresponds to the supplied filename
  - filename
  - tags to add
- `remove-tag`: remove specified tags from the entry that corresponds to the supplied filename
  - filename
  - tags to remove
- `filter`: lists the books that correspond to the filter input, sorted by last name of the first author
  - filter object, see below

## Filter
- "-a" -> author
- "-T" -> title 
- "-t" -> tags

## Remark on specifying a book as an argument
For simplicity's sake the way to specify a database entry is going to happen via the file name, as that is the only unique key. This is not ideal, since the goal of this program is to remove the hassle of having to work with filenames directly, but we'll have to make do with it for now.

The fields are matched using a `textMatcher` function. This currently just checks whether the input string is the prefix of any of the entry, but in the future I want to make it fuzzy.

> Example: `lits filter -a hartsho -T alg -t maths ag`

## Input validation
Whenever a filename is given as input, we want to validate that the file is actually in the current working directory.
