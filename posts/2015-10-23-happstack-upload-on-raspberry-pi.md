---
title: A Self-Hosted Inbox for Large Files Power by Happstack and The Raspberry Pi
description: How to collect pictures, movies and other files from family and friends with a self-hosted solution
tags: haskell
---


## How to Collect Files from Family and Friends after a Wedding or a Holiday Trip?

I have found myself repeatedly in a situation where I wanted to collect photos, videos, or some other files from my family and friends: Think wedding, or holiday trip! For this, I see two principal options:

* There are freemium cloud services like [Dropbox](https://www.dropbox.com), [Box](https://www.box.com), or [Google Drive](https://www.google.com/drive/).
* And there are many, many options for peer-to-peer file sharing, for instance [Bittorrent](https://en.wikipedia.org/wiki/Bittorrent).

As much as I like the concept of peer-to-peer communications, this option is outright too complicated for many less tech-savvy family members and friends. And even the cloud-based option has a couple of downsides:

* First, you let a company host your data, and depending on the service, you may even give up some rights. I like to avoid that if possible.
* Second, everybody who uploads pictures etc. needs to register at that site. This is simple for the young folks, but for collecting pictures from family members of all generations after a wedding, for instance, the technical barrier for sharing a folder of pictures should be as low as possible.
* And finally, if you want to collect a lot of data (25 GB seems to be the maximum that is available for free at the moment), you need to pay.

Well, there are other solutions! The one I wanted to explore and which I am going to explain in the following is based on a self-hosted solution using a very small and energy efficient computer, the [Raspberry Pi](https://www.raspberrypi.org/), a large hard drive, and a very simple web server written in the Haskell web framework [Happstack](http://www.happstack.com).

## A Simple Upload Form Based on Happstack and Resumable.js

When I was looking for a suitable web framework, I explored several options but ended up to use Happstack Lite because I could compile it directly on the Raspberry Pi (model B+, running ARM v6). In comparison to other frameworks that I tried ([Snap](http://snapframework.com/), [Yesod](http://www.yesodweb.com/), [Scotty](https://github.com/scotty-web/scotty)), I managed to actually compile [Happstack Lite](http://www.happstack.com/page/view-page-slug/9/happstack-lite) on [Raspbian Jessie](https://www.raspberrypi.org/downloads/raspbian/), which comes with GHC 7.6.3 (but no support for Template Haskell).

The source code of a simple demo server is located at my [Github repository](https://github.com/bdoering/hs-pi-upload). This basically provides the server functionality to serve and exploit the excellent [Resumable.js](http://www.resumablejs.com/) Javascript library. Resumable.js runs on modern browsers and makes use of the HTML5 File API. It allows to chunk large files on the client so that the server never has to handle files larger than e.g. 1 MB. A perfect fit for the small processing power of the Raspberry Pi! Also, the library allows a client to resume an upload should the connection be interrupted. Great for uploading all these large videos from the wedding over an unreliable Internet connection ;-)

To install the webserver on Raspbian Jessie, do:

#. Log into the Raspberry Pi.
#. Install some prerequisites, something along the lines of:
```
sudo apt-get install ghc git
```
#. Get the server source code from git:
```
git clone https://github.com/bdoering/hs-pi-upload.git
```
#. Edit the configuration at `hs-pi-upload/src/Config.hs` (which for the sake of simplicity is just a simple Haskell file). You will probably want to set up the upload directories to point to a location on an external hard drive. Just make sure that the paths actually exist.
#. Then compile the source code:
```
# cd to the directory containing the file "hs-pi-upload.cabal"
cabal sandbox init
cabal update
cabal install # Get lots of coffee now, the Pi is slow!
```
#. Finally, you can run the server with:
```
./.cabal-sandbox/bin/hs-pi-upload-exe
```
   This will start the server on port 8000 (the [Happstack Lite](http://www.happstack.com/page/view-page-slug/9/happstack-lite) default).

The actual upload form is served statically, and the file is actually the nearly unmodified demo from <https://github.com/23/resumable.js/blob/master/samples/java/src/main/webapp/index.html>. The server's role is to check for each uploaded chunk if the chunk has already been uploaded, and upon completion of all chunk uploads, to reassemble the files into the final file.

## Caveat

The web server is admittedly very simple and has the following shortcomings (or features, depending on how you want to see it):

* No protection of the upload form (anybody with the link can upload something). Most easy workaround would be to run the Happstack server behind another web server such as Nginx, and to enable basic authentication there.
* Existing files are overwritten (which is good if somebody forgot something and wants to upload the (zip) file again.

## Some Notes on the Server Setup

Here are some more tips on how you may run the Happstack server on the Raspberry Pi and to make it reachable over the Internet. Of course, you will also need to modify the settings of your router so that port 80 is actually forwarded to the Raspberry Pi.

### Free DNS

If you have a simple DSL connection like me, the publicly visible IP address of the Raspberry Pi will change regularly. So, we need a DNS entry. If you don't want to pay and are OK with a "random" name for your server, there are several free services available. I personally like [FreeDNS](https://freedns.afraid.org/) a lot as it allows me to set up more than one DNS entry for free easily.

Once you have registered, you can set up a *subdomain* (a *Type A* entry). If you then go to the page *Dynamic DNS*, you can find a `wget` command which you can add as an entry to the `crontab` running on the Raspberry Pi so that the DNS entry is updated automatically. My crontab entry (user `root`) looks something like this:

```
*/5 * * * * wget -q --read-timeout=0.0 --waitretry=5 --tries=400 --background http://freedns.afraid.org/dynamic/update.php?SEKRETKEYabcdefg1234567890ABCDEFGHIKKLM= -O /dev/null
```


### Nginx configuration

Putting the Happstack upload server behind an [Nginx](http://nginx.org/) server allows to easily run more than one server on the same machine. Also, we can easily define rewrite rules.

In this case, I just added the following server configuration to `/etc/nginx/sites-enabled/default`:

```
# Happstack server, running on port 8000
server {
      listen 80;
      server_name MYDNSNAME.mooo.com; # mooo.com is provided by FreeDNS

      rewrite ^/upload$ /static/html/upload.html redirect;

      location / {
               proxy_pass http://localhost:8000;
               proxy_redirect off;
               proxy_set_header Host $host;
               proxy_set_header X-Real-IP $remote_addr;
               proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
               client_max_body_size 10m;
               client_body_buffer_size 128k;
               proxy_connect_timeout 90;
               proxy_send_timeout 90;
               proxy_read_timeout 90;
               proxy_buffer_size 4k;
               proxy_buffers 4 32k;
               proxy_busy_buffers_size 64k;
               proxy_temp_file_write_size 64k;
      }
}
```

Make sure to restart Nginx with `service nginx restart` after you modified the configuration.


### Supervisor configuration

The HTML server should be started automatically whenever the Raspberry Pi reboots. Also, the server should be restarted automatically if it crashes. For this we can use [Supervisor](http://supervisord.org/), a program which allows to monitor and control other processes.

My setup for the Happstack server looks very simple. The file `/etc/supervisor/conf.d/hs-pi-upload.conf` just contains the following:

```
[program:hs-pi-upload]
command=/path/to/sandbox/hs-pi-upload/.cabal-sandbox/bin/hs-pi-upload-exe
user=pi
stdout_logfile_backups=2
stdout_logfile_maxbytes=20MB
```

To apply the changes, I restart Supervisor with `service supervisor restart`.


## Conclusions

Running a compiled Haskell web server on the Raspberry Pi resulted in a surprisingly good performance. If you have a good DSL connection, uploads to this server are basically as fast as to any other cloud based services such as Dropbox. Pretty cool! And all that running on such humble hardware, powered 100 % by open source software.

But keep in mind: Now that *you* host the data, you also need to care for backups and make sure that the server keeps running — otherwise you'll get calls quickly once you gave the upload link to some friends ;-) 

Happy uploading!
