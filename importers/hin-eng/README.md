This importer is for the hin-eng dictionary only. 
The dictionary was created by the Computation for Indian Language Technology (CFILT) at the 
Indian Institute of Technology, Bombay. 
The dictionary is called The Hindi Universal Word (UW) and is released under GNU FDL.

# Dependencies: 
- uv
- requests library

# Usage: 
```bash 
uv venv .venv --python 3.14 && source .venv/bin/activate && uv pip install requests
python hin-eng.py
```

# Functionality:
The script will automatically download the dictionary from [https://www.cfilt.iitb.ac.in/~hdict/webinterface_user/download.php?get=UW_Hindi_Dict_20131003.zip](https://www.cfilt.iitb.ac.in/~hdict/webinterface_user/download.php?get=UW_Hindi_Dict_20131003.zip)
and create a .tei file. 

# Formatting problems in original file that get fixed by the parser
- a few linebreak even though there should be none
- attributes/pos-tags sometimes have a "." which should be ","
- one time V-tag is spelled small.
- one time certain tags are spelled small (ADj instead of ADJ): i am
  assuming they are the same
- missing 67ish pos-tags: tei file gets filled with empty pos tag
- sometimes H,0,0> instead of <H,0,0>
- Original line format changes sometimes
```
[HINDIWORD] {} "ENGLISHWORD(SEMANTIC_RULES)" (TAGS) <H,0,0>;SAMPLE
[HINDIWORD] {} "ENGLISHWORD" (SEMANTIC_RULES) <H,0,0>;
[HINDIWORD] {} "ENGLISHWORD" (TAGS) <H,0,0>;
[HINDIWORD] {} "ENGLISHWORD(SEMANTIC_RULES)" (OTHER_SEMANTIC_RULES) <H,0,0>;'
[HINDIWORD] {}"ENGLISHWORD(TAGS)H,0,0>;SAMPLE
```
which made parsing complex and will probably be difficult to understand/debug.

# Questions

1. Some hindi words occur multiple times with the same surface string and have multiple
senses / different translations, but some semantic rules/restrictions (e.g. obj>school) are different.
For now I just added all the translations and added a <usg
type="indicator"> with the semantics/contexts. A possible next step would
be to group the same translations together and have multiple <usg>-tags? How
should this be handled?
2. The dictionary is hin-eng. Can I also write a script that switches it to eng-hin?
3. There are 40 empty Hindi word entries, that only have an english translation
I am skipping them. is that fine?
4. some entries have "***" in the string. I am not changing that but it adds noise.


# Include unused information
- a lot of semantics/context restrictions are still unused. if a word is only used in
  certain contexts (currently as <usg> ) and needs further parsing 
- a lot of custom pos-tags /custom tags: INTJ ADV-PHRASE INDC are also available.
