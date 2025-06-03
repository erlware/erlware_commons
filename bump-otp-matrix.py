__author__ = "Ariel Otilibili <otilibil@eurecom.fr>"
__copyright__ = "Copyright (c) 2025 Ariel Otilibili"
__license__ = "MIT"

import json
import requests
import re

from bs4 import BeautifulSoup

URL = "https://packages.debian.org/search?keywords=erlang"
WORKFLOW = ".github/workflows/main.yml"


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
    HEADER = "Concurrent, real-time, distributed functional language"
    OTP_REGEX = r"1:(\d+)"

    versions = {
        m.group(1)
        for p in webpage.find_all("li")
        if re.search(HEADER, p.text)
        and not re.search("security", p.text)
        and (m := re.search(OTP_REGEX, p.text))
    }

    return sorted(list(versions), reverse=True)


def update_otp_versions(versions, workflow):
    otp_versions = ", ".join(f"'{version}'" for version in versions)

    with open(workflow) as ci:
        conf = ci.read()

    conf = re.sub("(otp_version: ).*\n", "\\1" + f"[{otp_versions}]" + "\n", conf)

    with open(workflow, "w") as ci:
        ci.write(conf)


if __name__ == "__main__":
    webpage = get_webpage_in_html(URL)
    versions = get_latest_otp_versions(webpage)
    update_otp_versions(versions, WORKFLOW)

    print(json.dumps({"source": URL, "versions": versions, "file-changed": WORKFLOW}))
