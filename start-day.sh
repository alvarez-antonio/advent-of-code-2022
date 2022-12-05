day=Day$(printf "%02d" $1) 

cd app

aoc download -d $1 -I -i inputs/$day-input --session-file .adventofcode.session
touch $day.hs

# aoc read -d $1