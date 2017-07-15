"""This is a quickly hacked module for the Sopel IRC bot. It executes the dict
command line client to query available dictionaries. Using a proper dict library
would be far better and has not been done due to time constraints.

(C) 2017 Sebastian Humenda, licensed under the terms of the GPL, version 3, or
any later version, published by the FSF."""


import os
import random
import re
import subprocess
import sys

from sopel import module

dict_pattern = re.compile(r"^[a-z]{3}-[a-z]{3}$")
dict_file_pattern = re.compile(r'^freedict-([a-z]{3}-[a-z]{3}).index$')


def answer(bot, trigger, msg):
    if trigger.is_privmsg:
        bot.say(msg)
    else:
        bot.reply(msg)

def error(bot, msg):
    """Print an error message"""
    bot.reply(msg)


def lookup(bot, language, word):
    if language:
        if not dict_pattern.search(language):
            error(bot, "Invalid dictionary specifier.")
            return
    if len(word) > 20:
        error(bot, "Sorry, this word is too long.")
        return
    proc = None
    if language:
        proc = subprocess.Popen(['dict',  '-d', 'fd-' + language,
            word.encode(sys.getdefaultencoding())], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        proc = subprocess.Popen(['dict', word.encode(sys.getdefaultencoding())],
                stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    output = proc.communicate()
    if proc.wait():
        # try to get alternate suggestions
        stderr = output[1].decode(sys.getdefaultencoding())
        if 'perhaps you mean' in stderr:
            suggestions = '; '.join(stderr.split('\n')[1:])
            suggestions = suggestions[suggestions.find(':')+2:]
            bot.say("Sorry, entry not found, try: " + suggestions)
        else:
            bot.say("Sorry, but unfortunately this entry is not available.")
        return
    else:
        # strip first two lines
        text = output[0].decode(sys.getdefaultencoding()).lstrip()
        text = (l for l in text.split('\n')[3:] if l)
        text = [line for line in text if not line.lstrip().startswith("From ")]
        if len(text) > 5: # ToDo: use paste service
            text = text[:6]
            text[-1] += '  … a few more lines omitted.'
            if not language:
                text[-1] = text[-1][:-1] + ', try restricting the search by ' +\
                    'specifying a dictionary.'
        for line in text:
            bot.say(line)

################################################################################
# actual triggers

@module.rate(server=0.4)
@module.commands('dict')
def dict_lookup(bot, trigger):
    def report_invalid_input():
        answer(bot, trigger, ("Usage: .dict [DICTIONARY] word…; try .help dict "
            "for an explanation."))

    if not trigger.group(2):
        report_invalid_input()
        return

    data = trigger.group(2).split(' ')
    word = language = None
    if len(data) == 0:
        report_invalid_input()
        return
    elif len(data) == 1:
        word = data[0]
    else:
        language = data[0]
        word = ' '.join(data[1:])

    lookup(bot, language, word)


@module.rate(server=0.2)
@module.commands('list')
def list_dicts(bot, trigger):
    files = ', '.join(sorted(map(lambda d: d.groups()[0],
            filter(bool, map(dict_file_pattern.search,
                os.listdir('/usr/share/dictd'))))))
    answer(bot, trigger, "Available databases: " + files)


@module.commands('help')
def help_user(bot, trigger):
    args = trigger.group(2)
    if args:
        if args == '.list' or args == 'list':
            answer(bot, trigger, "Type .list and I will present you with a list of all FreeDict dictionaries.")
        elif args == '.dict' or args == 'dict':
            answer(bot, trigger, ("Type .dict <dictionary> <phrase> to "
                "look up something in the desired dictionary (see .list)."))
            answer(bot, trigger, "Alternatively, you can try .dict <word>, " + \
                    "but I'll only output the first matches, if too many dictionaries contain this word.")
            answer(bot, trigger, "For instance, why not try: " + \
                    random.choice(['.dict lat-deu sapientia',
                    '.dict deu-eng Hase',
                    '.dict fra-eng ordinateur',
                    '.dict eng-spa house']))
        else:
            answer(bot, trigger, "I am afraid this is something that " + \
                    "I am not entitled to know.")
    else: # general help
        answer(bot, trigger, ("I'm serving your dictionary queries. Try "
            "the command .list to get a list of dictionaries and the command "
            ".dict to look up a word. Use .help <command> to get information on the usage."))


