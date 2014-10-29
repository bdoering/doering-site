#!/bin/sh

cp ../doering-site/_site/* -r .
git add .
git commit -a -m "Publish"
git push

