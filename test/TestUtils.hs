module TestUtils where

import DataBase

{- The test database consists of the following entries:
Little Women - Louise May Alcott (american, english, novel, women)
The Last of the Mohicans - James Fenimore Cooper (american, english, indian, novel)
Absalom, Absalom! - William Faulkner (american, english, gothic, novel, southern)
The Great Gatsby - F. Scott Fitzgerald (american, americandream, english, novel)
The Scarlett Letter - Nathaniel Hawthorne (american, english, novel, shame)
Blood Meridian - Cormac McCarthy (american, english, novel, southern, western)
Moby-Dick - Herman Melville (american, english, ishmael, novel, whale)
The Grapes of Wrath - John Steinbeck (american, california, english, famine, novel)
Uncle Tom's Cabin - Harriet Beecher Stowe (american, blacks, english, novel, slaves)
Adventures of Huckleberry Finn - Mark Twain (adventure, american, english, novel) -}
testDB :: [Book]
testDB = [Book {fileName = "blood-meridian_cormac-mccarthy.epub", title = "Blood Meridian", author = [Author {firstName = Just "Cormac", lastName = "McCarthy"}], tags = ["english","novel","american","western","southern"]},Book {fileName = "the-grapes-of-wrath_john-steinbeck.epub", title = "The Grapes of Wrath", author = [Author {firstName = Just "John", lastName = "Steinbeck"}], tags = ["famine","california","english","novel","american"]},Book {fileName = "absalom-absalom_william-faulkner.epub", title = "Absalom, Absalom!", author = [Author {firstName = Just "William", lastName = "Faulkner"}], tags = ["gothic","southern","novel","english","american"]},Book {fileName = "the-great-gatsby_f-scott-fitzgerald.epub", title = "The Great Gatsby", author = [Author {firstName = Just "F. Scott", lastName = "Fitzgerald"}], tags = ["americandream","novel","english","american"]},Book {fileName = "adventures-of-huckleberry-finn_mark-twain.epub", title = "Adventures of Huckleberry Finn", author = [Author {firstName = Just "Mark", lastName = "Twain"}], tags = ["adventure","english","novel","american"]},Book {fileName = "little-women_louisa-may-alcott.epub", title = "Little Women", author = [Author {firstName = Just "Louise May", lastName = "Alcott"}], tags = ["women","novel","english","american"]},Book {fileName = "uncle-toms-cabin_harriet-beecher-stowe.epub", title = "Uncle Tom's Cabin", author = [Author {firstName = Just "Harriet Beecher", lastName = "Stowe"}], tags = ["slaves","blacks","american","novel","english"]},Book {fileName = "moby-dick_herman-melville.epub", title = "Moby-Dick", author = [Author {firstName = Just "Herman", lastName = "Melville"}], tags = ["ishmael","whale","novel","american","english"]},Book {fileName = "the-scarlett-letter_nathaniel-hawthorne.epub", title = "The Scarlett Letter", author = [Author {firstName = Just "Nathaniel", lastName = "Hawthorne"}], tags = ["shame","american","novel","english"]},Book {fileName = "the-last-of-the-mohicans_j-f-cooper.epub", title = "The Last of the Mohicans", author = [Author {firstName = Just "James Fenimore", lastName = "Cooper"}], tags = ["novel","american","english","indian"]}]

bookInList :: String -> [Book] -> Bool
bookInList _target [] = False
bookInList target (b : bs) = (target == fileName b) || bookInList target bs

booksAllInList :: [String] -> [Book] -> Bool
booksAllInList target books = all (`bookInList` books) target
