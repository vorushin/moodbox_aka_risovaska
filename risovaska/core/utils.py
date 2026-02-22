from datetime import datetime

def get_birthdate_from_age(age):
    now = datetime.now()
    try: # raised when birth date is February 29 and the current year is not a leap year
        return now.replace(year=now.year-age)
    except ValueError:
        return now.replace(year=now.year-age, day=now.day-1)
        
def get_integer_or_none(value):
    try:
        return int(value)
    except (ValueError, TypeError):
        return None