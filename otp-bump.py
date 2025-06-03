__author__ = "Ariel Otilibili <otilibil@eurecom.fr>"
__copyright__ = "Copyright (c) 2025 Ariel Otilibili"
__license__ = "MIT "

import requests
import re
from bs4 import BeautifulSoup

URL = "https://packages.debian.org/search?keywords=erlang"
HEADER = "Concurrent, real-time, distributed functional language"
OTP_REGEX = "1:(\d+)"


def get_webpage_in_html(url):
    response = requests.get(url)
    return BeautifulSoup(response.content)


def get_latest_otp_versions(webpage):
    # Samples of expected 'li' elements:
    #
    # <li class="bullseye"><a class="resultlink" href="/bullseye/erlang">bullseye (oldstable)</a> (interpreters):
    #         Concurrent, real-time, distributed functional language
    #
    #       <br/>1:23.2.6+dfsg-1+deb11u2 [<strong class="pmarker" title="">security</strong>]: all
    #
    #
    #     </li>
    # <li class="bookworm"><a class="resultlink" href="/bookworm/erlang">bookworm (stable)</a> (interpreters):
    #         Concurrent, real-time, distributed functional language
    #
    #       <br/>1:25.2.3+dfsg-1+deb12u1: all
    #
    #
    #     </li>

    versions = {
        f"'{m.group(1)}'"
        for p in webpage.find_all("li")
        if re.search(HEADER, p.text)
        and not re.search("security", p.text)
        and (m := re.search(OTP_REGEX, p.text))
    }

    otp_versions = ", ".join(sorted(list(versions), reverse=True))

    return f"[{otp_versions}]"


def update_otp_versions(versions):
    with open(".github/workflows/main.yml") as ci:
        conf = ci.read()

    conf = re.sub("(otp_version: ).*\n", "\\1" + versions + "\n", conf)

    with open(".github/workflows/main.yml", "w") as ci:
        ci.write(conf)


if __name__ == "__main__":
    print(f"Retrieving OTP versions from {URL}")
    webpage = get_webpage_in_html(URL)
    versions = get_latest_otp_versions(webpage)
    print(versions)
    update_otp_versions(versions)
