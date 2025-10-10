#!/bin/sh
# hledger-emacs.sh
# Recommended helper from https://hledger.org/editors.html
# Kudos to acarrico for the original script.

# The script is contingent on ledger-default-date-string
# being set to "%Y-%m-%d", aka ISO date format.

iargs=("$@")
oargs=()
j=0;
date=;
for((i=0; i<${#iargs[@]}; ++i)); do
    case ${iargs[i]} in
        --date-format)
            # drop --date-format and the next arg
            i=$((i+1));
            ;;
        cleared) # for ledger-di
            splay-balance-at-point
            # convert "cleared" to "balance -N -C"
            oargs[j]=balance; oargs[j+1]=-N; oargs[j+2]=-C; j=$((j+3));
            ;;
        xact)
            # convert "xact" to "print --match"
            oargs[j]=print; oargs[j+1]=--match; j=$((j+2));
            # drop xact argument and stash the date argument
            i=$((i+1));
            date=${iargs[i]};
            ;;
        # NOTE: Reconciliation still doesn't work for other reasons
        #       so the following filters/conversions are unnecessary for now.
        # --sort) # for reconcilliation
        #     # drop --sort and the next arg
        #     i=$((i+1));
        #     ;;
        # --uncleared) # for reconcilliation
        #     # convert "--uncleared" to "--unmarked --pending"
        #     oargs[j]=--unmarked; oargs[j+1]=--pending; j=$((j+2))
        #     ;;
        *)
            # keep any other args:
            oargs[j]=${iargs[i]};
            j=$((j+1));
            ;;
    esac
done

if test "$date"
then
    # substitute the given date for the old date:
    hledger "${oargs[@]}" | sed "1s/....-..-../$date/"
else
    # echo "${oargs[@]}"
    hledger "${oargs[@]}"
fi
