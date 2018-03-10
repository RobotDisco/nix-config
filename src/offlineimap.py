import re

def nametrans_local2gmail(folder):
    return {
        'drafts':    '[Gmail]/Drafts',
        'chats':     '[Gmail]/Chats',
        'flagged':   '[Gmail]/Starred',
        'important': '[Gmail]/Important',
        'spam':      '[Gmail]/Spam',
        'trash':     '[Gmail]/Trash',
        'sent':      '[Gmail]/Sent Mail',
        'archive':   '[Gmail]/All Mail',
        'inbox':     'INBOX'
    }.get(folder, folder)

def nametrans_gmail2local(folder):
    return  re.sub('\[Gmail\]\/Drafts', 'drafts',
            re.sub('\[Gmail\]\/Chats', 'chats',
            re.sub('\[Gmail\]\/Starred', 'flagged',
            re.sub('\[Gmail\]\/Important', 'important',
            re.sub('\[Gmail\]\/Spam', 'spam',
            re.sub('\[Gmail\]\/Trash', 'trash',
            re.sub('\[Gmail\]\/Sent Mail', 'sent',
            re.sub('\[Gmail\]\/All Mail', 'archive',
            re.sub('INBOX', 'inbox', folder)))))))))
