day=Day$(printf "%02d" $1) 

aoc download -d $1 -I -i inputs/$day-input --session-file .adventofcode.session
touch app/$day.hs

# aoc read -d $1