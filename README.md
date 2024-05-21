# LiTS -- Library Tagging System

LiTS is a commandline tool for tagging and based on these tags querying a collection of digital books.

# Commands
- `init`: Initialise lits in the current directory with an empty database (`.lits.json`) file. 
- `list`: List all entries in the database sorted by the last name of the first author.
- `add`: Add entry for the supplied file to the database. Prompts for title, author and tags.
- `delete`: Delete an entry from the database. If a corresponding file is present in the directory, delete that too.
- `import`: Check if every file that might be a book based on its extension has an entry in the database. If not, lits offers to create one for it.
- `clean`: Check if for every entry in the database the corresponding file exists in the current folder. If not, lits offers to delete the entry.
- `add-tag`: Prompts to add tags to entry that corresponds to the supplied filename.
- `remove-tag`: Prompts to remove tags from the entry that correspond to the supplied filename.
- `filter`: List the books that match filter input, sorted by last name of the first author. (See the section on filter below.)

## Filter
The syntax for specifying a filter is the following: it consists of three possible flags, corresponding to the author, title and tags of a book. Zero or more of these flags might be supplied. Letter case is ignored.
- "-a" -> author; the string will be matched against the first xor last name of any of the authors specified for an entry
- "-T" -> title; the string will be matched against the title field of an entry
- "-t" -> tags; expectd a sequence of tags that consist of alphanumeric characters and dashes (`-`) delimited by spaces. These are then matched against the set of tags associated to an entry.

The fields are matched using a `textMatcher` function. This currently just checks whether the **input string is the prefix of the field** for any of the entried, but in the future I want to make it fuzzy.

> Example: `lits filter -a Melv -T moby -t novel whale`
