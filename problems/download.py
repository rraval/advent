import click
import requests

from html2text import html2text
from lxml import etree
from pathlib import Path

def getDescription(day):
    html = requests.get('http://adventofcode.com/day/{}'.format(day)).content
    tree = etree.HTML(html)
    article = tree.xpath('//article[@class="day-desc"]')[0]

    desc_html = ''.join(etree.tostring(elem).decode('utf-8') for elem in article)
    return html2text(desc_html)

def getInput(cookie, day):
    resp = requests.get(
        'http://adventofcode.com/day/{}/input'.format(day),
        headers={'Cookie': cookie},
    )
    return resp.content.decode('utf-8')

@click.command()
@click.argument('cookie')
def main(cookie):
    """
    Downloads problem descriptions and inputs and puts them in separate
    directories labelled by day.

    Since inputs are customized on a per user basis, you'll have to login with
    your browser and figure out the cookie that the website uses to
    authenticate you. It generally looks like `session=<hex string>`.
    """

    for day in range(1, 26):
        print("Day {}".format(day))
        daydir = Path('day{:02d}'.format(day))
        daydir.mkdir(exist_ok=True)

        with (daydir / 'description.md').open('w') as fp:
            fp.write(getDescription(day))

        with (daydir / 'input.txt').open('w') as fp:
            fp.write(getInput(cookie, day))

if __name__ == '__main__':
    main()
