for i in (seq 1 10)
    echo $i > out.txt
    git commit -a -m $i
    rm out.txt
    git commit -a -m $i
end