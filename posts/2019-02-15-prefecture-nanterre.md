---
title: Tricking the Prefecture's Appointment Reservation System
description: Securing a rare appointment at the Prefecture Nanterre
tags: SAR, ERCS, metrology
noToc: false
---

# The Challenge

Applying for dual citizenship in France is a lengthy process. If you live around Paris in the department [*Hauts-de-Seine*](http://www.hauts-de-seine.fr/), your first interaction with the French state will be the *pre accueil* (introductory meeting) if you apply for a [naturalization by declaration](http://www.hauts-de-seine.gouv.fr/Demarches-administratives/Etrangers-en-France/Naturalisation) -- marriage (*naturalisation par déclaration (mariage))*. 

The challenge? You will have to [get an appointment online](http://www.hauts-de-seine.gouv.fr/Prendre-un-rendez-vous), but basically all places are always taken for weeks. If you cannot even hand in your documents, you are certainly not getting very far concerning your dual citizenship. 

Is this a little scam of the French state to limit the number of applicants? They seem to have found a rather clever and indirect way of achieving this...


# The Solution

So what can you do? Either you regularly check their website to see if a new place is available, or you write a little program which does it for you. I did the second.

Basically, when you want to reserve a place, you are confronted with this [Module de réservation](http://www.hauts-de-seine.gouv.fr/booking/create/4485), an online form.

After checking the checkbox (acceptance of conditions) and clicking on *Effectuer une demande de rendez-vous*, you will see the following message:

> Il n'existe plus de plage horaire libre pour votre demande de rendez-vous. Veuillez recommencer ultérieurement. 

Bummer.

This goes on for days.

And days.

And weeks.

Rechecking this registration page gets old rather quickly. The little Python script below instead does it for you. Should a place be available, it will simply emit a beeping sound. The program can therefore stay running in the background and whenever a place is available, an alarm goes off. 

```python
import datetime
import os
import requests
import sys
import time

URL = "http://www.hauts-de-seine.gouv.fr/booking/create/4485/0"
# We need to look like a normal browser because otherwise we are blocked
headers = {
    'User-Agent': 'Mozilla/5.0 (X11; Fedora; Linux x86_64; rv:64.0) ' \
    'Gecko/20100101 Firefox/64.0'
}

checkPhrase = "Il n'existe plus de plage horaire libre pour votre " \
    "demande de rendez-vous."

thisDir = os.path.dirname(__file__)

def checkPlacesAreAlreadyTaken():
    with requests.Session() as s:
        r = s.get(URL, headers=headers)
        r = s.post(URL, headers=headers,
                   data={'condition':'on',
                         'nextButton':'Effectuer+une+demande+de+rendez-vous'})
    return checkPhrase in r.text

def writeResults(placesAreAlreadyTaken):
    """Log the result to STDOUT and to two different files."""
    now = datetime.datetime.now()
    print('{} All places are already taken: {}'.format(now, placesAreAlreadyTaken))
    if placesAreAlreadyTaken:
        with open(os.path.join(thisDir, 'placesAreAlreadyTaken.log'), 'a') as f:
            f.write('{} Places are taken\n'.format(now))
    else:
        with open(os.path.join(thisDir, 'placeIsAvailable.log'), 'a') as f:
            f.write('{} At least one place is available!\n'.format(now))

def playSound():
    """Emit three terminal beeps within 1.5 s."""
    for i in range(3):
        sys.stdout.write('\a')
        sys.stdout.flush()
        time.sleep(0.5)

def main():
    print('Starting to monitor the reservation system.')
    print('Press CTRL+C to abort...\n')
    while True:
        try:
            placesAreAlreadyTaken = checkPlacesAreAlreadyTaken()
        except requests.exceptions.RequestException as e:
            sys.stderr.write('** Error while querying site: {}\n'.format(e))
        else:
            if not placesAreAlreadyTaken:
                playSound()
            writeResults(placesAreAlreadyTaken)
        time.sleep(10)  

if __name__ == '__main__':
    sys.exit(main())

```
This script was written for Python 3 and tested on Linux, but there is no reason why it should not work on Windows as well.

After about two weeks, a place became available and I could quickly register! Maybe the script is useful to somebody else facing the same problem? 




