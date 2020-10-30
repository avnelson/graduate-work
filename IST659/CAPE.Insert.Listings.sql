/*
ENTRY 
ONE*/
INSERT INTO cape_artist (artist_id, artist_username, shop_url, shop_description)
VALUES ('42069', 'Dummy', 'https://matias.ma/nsfw/', 'This is my shop')

INSERT INTO cape_shop_listing 
	(artist_id, shop_url, listing_description, listing_coverage_id,
	listing_finish_id, listing_media_id, listing_price_range_id)
VALUES ('42069', 'https://matias.ma/nsfw/', 'its not actually nsfw.', '1', '1', '1', '1')
GO
--insert weak associative values
INSERT INTO cape_subject_artist (wet_listing_id, wet_subject_id)
VALUES ('1', '1')
INSERT INTO cape_currency_artist (wet_listing_id, wet_currency_id)
VALUES ('1', '1')
INSERT INTO cape_keyword_artist (wet_listing_id, wet_keyword_id)
VALUES ('1', '1')
INSERT INTO cape_wnd_artist (wet_listing_id, wet_wnd_id)
VALUES ('1', '1')
GO

/*
ENTRY 
TWO*/
INSERT INTO cape_artist (artist_id, artist_username, shop_url, shop_description)
VALUES ('192632', 'Aflek', 'https://www1.flightrising.com/forums/adopt/2055721/1/', 'Insect Designs')

INSERT INTO cape_shop_listing 
	(artist_id, shop_url, listing_description, listing_coverage_id,
	listing_finish_id, listing_media_id, listing_price_range_id)
VALUES ('192632', 'https://www1.flightrising.com/forums/adopt/2055721/1', 'Chibi Adopts', '3', '4', '5', '1')
GO
--insert weak associative values
INSERT INTO cape_subject_artist (wet_listing_id, wet_subject_id)
VALUES ('2', '9')
INSERT INTO cape_currency_artist (wet_listing_id, wet_currency_id)
VALUES ('2', '1'), ('2', '2'), ('2', '3'), ('2', '6')
INSERT INTO cape_keyword_artist (wet_listing_id, wet_keyword_id)
VALUES ('2', '1')
INSERT INTO cape_wnd_artist (wet_listing_id, wet_wnd_id)
VALUES ('2', '1'), ('2', '2'), ('2','3'), ('2','4'),('2','5'), ('2','6'), ('2','7')
GO

/*
ENTRY 
THREE*/
INSERT INTO cape_artist (artist_id, artist_username, shop_url, shop_description)
VALUES ('69', 'Map', 'https://www.etsy.com/', 'Mapalicious Maps')

INSERT INTO cape_shop_listing 
	(artist_id, shop_url, listing_description, listing_coverage_id,
	listing_finish_id, listing_media_id, listing_price_range_id)
VALUES ('69', 'https://www.etsy.com/', NULL, '3', '6', '6', '2')
GO
--insert weak associative values
INSERT INTO cape_subject_artist (wet_listing_id, wet_subject_id)
VALUES ('4', '3'), ('4', '5')
INSERT INTO cape_currency_artist (wet_listing_id, wet_currency_id)
VALUES ('4', '1'), ('4', '2'), ('4', '3')
INSERT INTO cape_keyword_artist (wet_listing_id, wet_keyword_id)
VALUES ('4', '1'), ('4', '2')
INSERT INTO cape_wnd_artist (wet_listing_id, wet_wnd_id)
VALUES ('4', '1'), ('4', '2'), ('4','3'), ('4','7')
GO

/*
ENTRY 
FOUR*/
INSERT INTO cape_artist (artist_id, artist_username, shop_url, shop_description)
VALUES ('535353', 'Trampiline', 'https://www.tumblr.com/dashboard', ':)')

INSERT INTO cape_shop_listing 
	(artist_id, shop_url, listing_description, listing_coverage_id,
	listing_finish_id, listing_media_id, listing_price_range_id)
VALUES ('535353', 'https://www.tumblr.com/dashboard', 'The color of sound', '4', '5', '1', '1')
GO
--insert weak associative values
INSERT INTO cape_subject_artist (wet_listing_id, wet_subject_id)
VALUES ('5', '10')
INSERT INTO cape_currency_artist (wet_listing_id, wet_currency_id)
VALUES ('5', '3')
INSERT INTO cape_keyword_artist (wet_listing_id, wet_keyword_id)
VALUES ('5', '3'), ('5', '4'), ('5', '8')
INSERT INTO cape_wnd_artist (wet_listing_id, wet_wnd_id)
VALUES ('5', '2'), ('5', '3'), ('5','7')
GO

/*
ENTRY 
FIVE*/
INSERT INTO cape_artist (artist_id, artist_username, shop_url, shop_description)
VALUES ('56987', 'Googol', 'http://www.google.com/', 'I rule the internet and like it')

INSERT INTO cape_shop_listing 
	(artist_id, shop_url, listing_description, listing_coverage_id,
	listing_finish_id, listing_media_id, listing_price_range_id)
VALUES ('56987', 'http://www.google.com/', 'very beautiful art. the most beautiful.',
'3', '4', '5', '4')
GO
--insert weak associative values
INSERT INTO cape_subject_artist (wet_listing_id, wet_subject_id)
VALUES ('7', '2'), ('7', '5')
INSERT INTO cape_currency_artist (wet_listing_id, wet_currency_id)
VALUES ('7', '1'), ('7', '4'), ('7', '6')
INSERT INTO cape_keyword_artist (wet_listing_id, wet_keyword_id)
VALUES ('7', '1'), ('7', '3'), ('7', '11')
INSERT INTO cape_wnd_artist (wet_listing_id, wet_wnd_id)
VALUES ('7', '2'), ('7', '4')
GO

SELECT cape_wnd_artist.wet_wnd_id, cape_wnd_artist.wet_listing_id FROM cape_wnd_artist
WHERE cape_artist.artist_username='Aflek'

