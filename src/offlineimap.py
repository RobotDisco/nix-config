import re
import os
import subprocess

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


def decrypt_client_id():
    path = "{}/.passwd/offlineimap-oauth2-client-id.gpg".format(os.path.expanduser("~"))
    args = ["gpg", "--use-agent", "--quiet", "--batch", "-d", path]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ""

def decrypt_client_secret():
    path = "{}/.passwd/offlineimap-oauth2-client-secret.gpg".format(os.path.expanduser("~"))
    args = ["gpg", "--use-agent", "--quiet", "--batch", "-d", path]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ""

def decrypt_refresh_token(account):
    account = os.path.basename(account)
    path = "{}/.passwd/{}-oauth2-refresh-token.gpg".format(os.path.expanduser("~"), account)

    args = ["gpg", "--use-agent", "--quiet", "--batch", "-d", path]
    try:
        return subprocess.check_output(args).strip()
    except subprocess.CalledProcessError:
        return ""
