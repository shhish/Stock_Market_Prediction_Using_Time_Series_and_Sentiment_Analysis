from Scweet.scweet import scrape
from Scweet.user import get_user_information, get_users_following, get_users_followers

data = scrape(words=['amc'], since="2021-01-15", until="2021-08-09", from_account = 'reuters', interval=1, headless=False, display_type="Top", save_images=False, lang="en",
	resume=False, filter_replies=False, proximity=False)
    