compressed_size() {
    sudo tarsnap --humanize-numbers --print-stats | grep 'unique data' | awk '{print $5 " " $6}'
}

compressed_size_bytes() {
    sudo tarsnap --print-stats | grep 'unique data' | awk '{print $4}'
}

price_per_month() {
    byte_per_month_in_picodollar=250
    bytes=$(compressed_size_bytes)

    echo "scale=2; $bytes*$byte_per_month_in_picodollar/1000/1000/1000/1000" | bc
}
