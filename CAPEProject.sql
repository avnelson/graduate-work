/* 
REMOVE
ALL
TABLES
*/
DROP TABLE cape_wnd_artist
DROP TABLE cape_wnd_patron
DROP TABLE cape_keyword_artist
DROP TABLE cape_keyword_patron
DROP TABLE cape_currency_artist
DROP TABLE cape_currency_patron
DROP TABLE cape_subject_artist
DROP TABLE cape_subject_patron
DROP TABLE cape_artist_listing_index
DROP TABLE cape_shop_listing
DROP TABLE cape_artist
DROP TABLE cape_patron
DROP TABLE cape_wnd
DROP TABLE cape_keyword_index
DROP TABLE cape_price_range
DROP TABLE cape_currency_type
DROP TABLE cape_art_media
DROP TABLE cape_finish_type
DROP TABLE cape_coverage
DROP TABLE cape_subject
GO


/* 
CREATE
ALL
TABLES
*/
--subject tag table
CREATE TABLE cape_subject (
subject_id int identity primary key,
subject_name varchar(16) not null )
GO

--coverage tag table
CREATE TABLE cape_coverage (
coverage_id int identity primary key,
coverage_name varchar(16) not null )
GO

--finish type tag table
CREATE TABLE cape_finish_type (
finish_id int identity primary key,
finish_name varchar(16) not null )
GO

--art media type table
CREATE TABLE cape_art_media (
media_id int identity primary key,
media_name varchar(25) not null )
GO

--currency type table
CREATE TABLE cape_currency_type (
currency_id int identity primary key,
currency_name varchar(16) not null )
GO

--price range table
CREATE TABLE cape_price_range (
price_range_id int identity primary key,
price_range_name varchar(16) not null )
GO

--keyword index table
CREATE TABLE cape_keyword_index (
keyword_id int identity primary key,
keyword_name varchar(16) not null )
GO

--will not do table
CREATE TABLE cape_wnd (
wnd_id int identity primary key,
wnd_name varchar(16) not null)
GO

--patron table
CREATE TABLE cape_patron (
patron_id int primary key,
patron_username varchar(16) not null,
patron_search_description varchar(50),
patron_subject_id int not null,
patron_coverage_id int,
patron_finish_id int,
patron_media_id int,
patron_currency_id int not null,
patron_price_range_id int not null,
patron_keyword_id int,
patron_wnd_id int
--patron table constraints
CONSTRAINT FK1_cape_patron FOREIGN KEY (patron_subject_id)REFERENCES cape_subject (subject_id),
CONSTRAINT FK8_cape_patron FOREIGN KEY (patron_coverage_id)REFERENCES cape_coverage (coverage_id),
CONSTRAINT FK2_cape_patron FOREIGN KEY (patron_finish_id)REFERENCES cape_finish_type (finish_id),
CONSTRAINT FK3_cape_patron FOREIGN KEY (patron_media_id)REFERENCES cape_art_media (media_id),
CONSTRAINT FK4_cape_patron FOREIGN KEY (patron_currency_id)REFERENCES cape_currency_type (currency_id),
CONSTRAINT FK5_cape_patron FOREIGN KEY (patron_price_range_id)REFERENCES cape_price_range (price_range_id),
CONSTRAINT FK6_cape_patron FOREIGN KEY (patron_keyword_id)REFERENCES cape_keyword_index (keyword_id),
CONSTRAINT FK7_cape_patron FOREIGN KEY (patron_wnd_id)REFERENCES cape_wnd (wnd_id) )
GO

--artist table
CREATE TABLE cape_artist (
artist_id int primary key,
artist_username varchar(16) not null,
shop_url nvarchar(2083),
shop_description varchar(50))
GO

--shop listing table
CREATE TABLE cape_shop_listing (
listing_id int identity primary key,
artist_id int not null,
shop_url nvarchar(2083) not null,
listing_description varchar(50),
--listing_subject_id int not null,
listing_coverage_id int not null,
listing_finish_id int not null,
listing_media_id int not null,
--listing_currency_id int not null,
listing_price_range_id int not null,
--listing_keyword_id int not null,
--listing_wnd_id int
--shop listing table constraints
--CONSTRAINT FK1_cape_shop_listing FOREIGN KEY (listing_subject_id)REFERENCES cape_subject (subject_id),
CONSTRAINT FK9_cape_shop_listing FOREIGN KEY (listing_coverage_id)REFERENCES cape_coverage (coverage_id),
CONSTRAINT FK2_cape_shop_listing FOREIGN KEY (listing_finish_id)REFERENCES cape_finish_type (finish_id),
CONSTRAINT FK3_cape_shop_listing FOREIGN KEY (listing_media_id)REFERENCES cape_art_media (media_id),
--CONSTRAINT FK4_cape_shop_listing FOREIGN KEY (listing_currency_id)REFERENCES cape_currency_type (currency_id),
CONSTRAINT FK5_cape_shop_listing FOREIGN KEY (listing_price_range_id)REFERENCES cape_price_range (price_range_id),
--CONSTRAINT FK6_cape_shop_listing FOREIGN KEY (listing_keyword_id)REFERENCES cape_keyword_index (keyword_id),
--CONSTRAINT FK7_cape_shop_listing FOREIGN KEY (listing_wnd_id)REFERENCES cape_wnd (wnd_id),
CONSTRAINT FK8_cape_shop_listing FOREIGN KEY (artist_id) REFERENCES cape_artist(artist_id))
GO

--artist listing directory table
CREATE TABLE cape_artist_listing_index (
artist_listing_id int identity primary key,
artist_id int not null,
listing_id int not null
--directory contraints
CONSTRAINT FK1_cape_artist_listing_index FOREIGN KEY (artist_id) REFERENCES cape_artist(artist_id),
CONSTRAINT FK2_cape_artist_listing_index FOREIGN KEY (listing_id) REFERENCES cape_shop_listing(listing_id))
GO



/* 
WEAK ENTITY TABLES
*/
--subject/patron table
CREATE TABLE cape_subject_patron (
wet_patron_id int not null,
wet_subject_id int not null
--WET subject/patron table constraints
CONSTRAINT FK1_cape_subject_patron FOREIGN KEY (wet_subject_id)REFERENCES cape_subject (subject_id),
CONSTRAINT FK2_cape_subject_patron FOREIGN KEY (wet_patron_id)REFERENCES cape_patron (patron_id))
GO

--subject/artist table
CREATE TABLE cape_subject_artist (
wet_listing_id int not null,
wet_subject_id int not null
--WET subject/artist table constraints
CONSTRAINT FK1_cape_subject_artist FOREIGN KEY (wet_subject_id)REFERENCES cape_subject (subject_id),
CONSTRAINT FK2_cape_subject_artist FOREIGN KEY (wet_listing_id)REFERENCES cape_shop_listing (listing_id))
GO

--currency/patron table
CREATE TABLE cape_currency_patron (
wet_patron_id int not null,
wet_currency_id int not null
--WET currency/patron table constraints
CONSTRAINT FK1_cape_currency_patron FOREIGN KEY (wet_currency_id)REFERENCES cape_currency_type (currency_id),
CONSTRAINT FK2_cape_currency_patron FOREIGN KEY (wet_patron_id)REFERENCES cape_patron (patron_id))
GO

--currency/artist table
CREATE TABLE cape_currency_artist (
wet_listing_id int not null,
wet_currency_id int not null
--WET currency/artist table constraints
CONSTRAINT FK1_cape_currency_artist FOREIGN KEY (wet_currency_id)REFERENCES cape_currency_type (currency_id),
CONSTRAINT FK2_cape_currency_artist FOREIGN KEY (wet_listing_id)REFERENCES cape_shop_listing (listing_id))
GO

--keyword/patron table
CREATE TABLE cape_keyword_patron (
wet_patron_id int not null,
wet_keyword_id int not null
--WET keyword/patron table constraints
CONSTRAINT FK1_cape_keyword_patron FOREIGN KEY (wet_keyword_id)REFERENCES cape_keyword_index (keyword_id),
CONSTRAINT FK2_cape_keyword_patron FOREIGN KEY (wet_patron_id)REFERENCES cape_patron (patron_id))
GO

--keyword/artist table
CREATE TABLE cape_keyword_artist (
wet_listing_id int not null,
wet_keyword_id int not null
--WET keyword/artist table constraints
CONSTRAINT FK1_cape_keyword_artist FOREIGN KEY (wet_keyword_id)REFERENCES cape_keyword_index (keyword_id),
CONSTRAINT FK2_cape_keyword_artist FOREIGN KEY (wet_listing_id)REFERENCES cape_shop_listing (listing_id))
GO

--Will Not Draw/patron table
CREATE TABLE cape_wnd_patron (
wet_patron_id int not null,
wet_wnd_id int not null
--WET wnd/patron table constraints
CONSTRAINT FK1_cape_wnd_patron FOREIGN KEY (wet_wnd_id)REFERENCES cape_wnd (wnd_id),
CONSTRAINT FK2_cape_wnd_patron FOREIGN KEY (wet_wnd_id)REFERENCES cape_patron (patron_id))
GO

--Will Not Draw/artist table
CREATE TABLE cape_wnd_artist (
wet_listing_id int not null,
wet_wnd_id int not null
--WET wnd/artist table constraints
CONSTRAINT FK1_cape_wnd_artist FOREIGN KEY (wet_wnd_id)REFERENCES cape_wnd (wnd_id),
CONSTRAINT FK2_cape_wnd_artist FOREIGN KEY (wet_listing_id)REFERENCES cape_shop_listing (listing_id))
GO