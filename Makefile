with-target:
	@ansible-playbook --ask-become-pass -i inventories/live $(TARGET).yml --vault-password-file .vault-pass.txt

encrypt:
	@ansible-vault encrypt_string $(SECRET) --vault-password-file .vault-pass.txt
