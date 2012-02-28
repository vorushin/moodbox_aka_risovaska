-record(ec2_reservation_info, {reservation_id, owner_id, groups, instances}).

-record(ec2_instance, {ami_launch_index, dns_name, image_id, instance_id, instance_state, instance_type, key_name, kernel_id, launch_time, placement, private_dns_name, product_codes, ramdisk_id, reason}).

-record(ec2_group, {group_id}).
-record(ec2_instance_state, {code, name}).
-record(ec2_placement, {availability_zone}).
-record(ec2_product_code, {product_code}).
