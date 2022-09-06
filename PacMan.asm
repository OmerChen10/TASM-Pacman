IDEAL
MODEL small
STACK 10
p286 
LOCALS
INCLUDE "PacArt.inc"
DATASEG

distances dw 4 dup (0)
pixel_color db 0
winning_string db "YOU WON!$"
game_over_string db "GAME OVER!$"
respawning_string db "YOU DIED! PRESS ANY KEY$"
directions db 'U', 'D', 'L', 'R'

; ghost timing and modes
ghost_mode_periods db 20,7,20,5,20,5,0FFh ; FF is in the end to allow the ghost to operate in a chase mode forever.
current_mode_period db 7
mode_cycle db 1

clock_save db ?
clock db 1
frightened_game_cycle dw 0FFFFh

point_string db "Points: $"
points dw 0
point_counter db 0

pacman_posx dw 82
pacman_posy dw 104
direction dw 0
buffer_size equ 20
input_buffer db buffer_size dup (0)
prev_direction dw 0
pacman_lives db 2
pacman_style dw offset pacman_art_right

blinky_posx dw 110
blinky_posy dw 69
blinky_direction dw 0
blinky_mode db ?

pinky_posx dw 53
pinky_posy dw 69
pinky_direction dw 0
pinky_target_x dw 0
pinky_target_y dw 0
pinky_mode db ?

clyde_posx dw 53
clyde_posy dw 103
clyde_direction dw 0
clyde_target_x dw 0
clyde_target_y dw 0
clyde_mode db ?

inky_posx dw 110
inky_posy dw 103
inky_direction dw 0
inky_target_x dw 0
inky_target_y dw 0
inky_mode db ?

; in-game items

points_cords dw 3,4,11,4,19,4,27,4,35,4,44,4,53,4,63,4,73,4,91,4,100,4,110,4,119,4,128,4,136,4,144,4
			 dw 152,4,160,4,3,16,35,16,73,16,91,16,128,16,160,16,11,28,19,28,27,28,35,28,44,28,53,28,63,28,73,28,82,28,91,28,100,28,110,28,119,28,128,28,136,28,144,28,152,28,3,38,35,38,53,38,110,38,128,38,160,38,3,47
			 dw 11,47,19,47,27,47,35,47,53,47,63,47,73,47,91,47,100,47,110,47,128,47,136,47,144,47,152,47,160,47,35,56,128,56,35,65,128,65,35,74,128,74,35,83,128,83,35,91,128,91,35,99,128,99,35,107,128,107,35,115,128,115,3,123,11,123,19,123,27,123,35,123,44,123,53,123,63,123,73,123,91,123,100,123,110,123,119,123,128,123,136,123,144,123,152,123,160,123,3,133,35,133,73,133,91,133,128,133,160,133,10,142,16,142,35,142,44,142,53,142,63,142,73,142,82,142,91,142,100,142,110,142,119,142,128,142,147,142,153,142,16,151,35,151,53,151,110,151,128,151,147,151,3,160,11,160,19,160,27,160,35,160,53,160,63,160,73,160,91,160,100,160,110,160,128,160,136,160,144,160,152,160,160,160,3,169,73,169,91,169,160,169,3,179
			 dw 11,179,19,179,27,179,35,179,44,179,53,179,63,179,73,179,82,179,91,179,100,179,110,179,119,179,128,179,136,179,144,179,152,179,160,179
;end	
NUMBER_OF_POINTS EQU 163

power_pellet_cords dw 3,28, 160, 28, 3, 142, 160, 142
NUMBER_OF_POWER_pelletS EQU 4

CODESEG

; general procedures

proc print_asset

	@@ASSET_OFFSET EQU [bp+12]
	@@ASSET_HEIGHT EQU [bp+10]
	@@ASSET_WIDTH EQU [bp+8]
	@@POSX EQU [word ptr bp+6]
	@@POSY EQU [word ptr bp+4]

	push bp
	mov bp, sp
	pusha

	mov di, @@ASSET_OFFSET
	mov cx, @@ASSET_HEIGHT

	@@drawY:
		push cx
		mov cx, @@ASSET_WIDTH
		@@drawX:
			push cx
			cmp [byte ptr di], 0FFh
			je @@continue

			mov ah, 0ch
			mov al, [byte ptr di]
			mov cx, @@POSX
			mov dx, @@POSY
			mov bh, 0
			int 10h  ; Prints a pixel at the correct cords.

			@@continue:
			inc @@POSX
			inc di
			pop cx 
			loop @@drawx
		
		mov cx, @@ASSET_WIDTH
		sub @@POSX, cx
		inc @@POSY
		pop cx
		loop @@drawY
		

	popa
	pop bp
	ret 10
endp print_asset

proc move_asset

	@@ASSET_OFFSET EQU [bp+10]
	@@CURR_POSX_POINTER EQU [bp+8]
	@@CURR_POSY_POINTER EQU [bp+6]
	@@DIRECTION EQU [bp+4]
	push bp
	mov bp, sp
	pusha 

	mov bx, @@CURR_POSY_POINTER   ; move to bx the offset of the current y postion of the asset.
	mov ax, @@DIRECTION   ; check which direction is wanted and jump to the correct label.
    cmp ax, 'U'
    je @@up
    cmp ax, 'D'
    je @@down     ; check if the wanted direcion is up / down.
	mov bx, @@CURR_POSX_POINTER   ; If the direction is not up / down, set bx with the offset of the current x position of the asset.
    cmp ax, 'L'
    je @@left
    cmp ax, 'R'
    je @@right
	
	jmp @@print   ; If none of the control keys have been pressed (which means that a non-relevant key has been pressed) skip to print.

	@@up:
        dec [word ptr bx]  ; change the position of the asset correctly.
        jmp @@print
    @@down:

        inc [word ptr bx]  ; change the position of the asset correctly.
        jmp @@print
    @@left:
		mov bx, @@CURR_POSX_POINTER
        dec [word ptr bx]
        jmp @@print
    @@right:
		mov bx, @@CURR_POSX_POINTER ; change the position of the asset correctly.
        inc [word ptr bx]
        jmp @@print
	
    @@print:
        mov ax, @@ASSET_OFFSET   ; loads the offset of the asset's array.
        push ax
		push 12
		push 12
		mov bx, @@CURR_POSX_POINTER  ; loads the calculated x positon to the procedure.
        push [word ptr bx]
		mov bx, @@CURR_POSY_POINTER  ; loads the calculated y positon to the procedure.
        push [word ptr bx] 
        call print_asset

    popa
    pop bp
    ret 8
endp move_asset

proc check_collision

	@@CURR_POSX EQU [bp+8]
	@@CURR_POSY EQU [bp+6]
	@@DIRECTION EQU [word ptr bp+4]

	push bp
	mov bp, sp
	pusha

	clc  ; clears the carry flag (resets the collision indicator)
	mov dx, @@DIRECTION
	cmp @@DIRECTION, 'U'
	je @@up
	cmp @@DIRECTION, 'D'
	je @@down
	cmp @@DIRECTION, 'L'
	je @@left
	cmp @@DIRECTION, 'R'
	je @@right


	@@up:
		mov dx, 0
		mov cx, 12
		@@check_above:
			mov bx, @@CURR_POSX
			add bx, dx
			push bx ; send the current x postion to the procedure.
			push @@CURR_POSY ; send the current y postion to the procedure.
			push 'U'   ; push 'U' to the procedure (which sets the direction that the next procedure will check).
			call get_pixel_ahead  ; calls a procedure who puts the next pixel's color in the var pixel_color (to check whether the path is clear).
			pop ax
			cmp ax, 1  
			je @@not_clear  ; if the next pixel is not black (0), don't move the asset and print the asset at the same loaction.
			inc dx
			loop @@check_above
		
		jmp @@clear

	@@down:
		mov dx, 0
		mov cx, 12
		@@check_below:
			mov bx, @@CURR_POSX
			add bx, dx
			push bx ; send the current x postion to the procedure.
			push @@CURR_POSY ; send the current y postion to the procedure.
			push 'D'   ; push 'D' to the procedure (which sets the direction that the next procedure will check).
			call get_pixel_ahead  ; calls a procedure who puts the next pixel's color in the var pixel_color (to check whether the path is clear).
			pop ax
			cmp ax, 1  
			je @@not_clear  ; if the next pixel is not black (0), don't move the asset and print the asset at the same loaction.
			inc dx
			loop @@check_below
		
		jmp @@clear

	@@left:
		mov dx, 0
		mov cx, 12
		@@check_left:
			push @@CURR_POSX ; send the current x postion to the procedure.
			mov bx, @@CURR_POSY
			add bx, dx
			push bx    ; send the current y postion to the procedure.
			push 'L'   ; push 'L' to the procedure (which sets the direction that the next procedure will check).
			call get_pixel_ahead  ; calls a procedure who puts the next pixel's color in the var pixel_color (to check whether the path is clear).
			pop ax
			cmp ax, 1  
			je @@not_clear  ; if the next pixel is not black (0), don't move the asset and print the asset at the same loaction.
			inc dx
			loop @@check_left
		
		jmp @@clear

	@@right:
		mov dx, 0
		mov cx, 12
		@@check_right:
			push @@CURR_POSX ; send the current x postion to the procedure.
			mov bx, @@CURR_POSY
			add bx, dx
			push bx    ; send the current y postion to the procedure.
			push 'R'   ; push 'R' to the procedure (which sets the direction that the next procedure will check).
			call get_pixel_ahead  ; calls a procedure who puts the next pixel's color in the var pixel_color (to check whether the path is clear).
			pop ax
			cmp ax, 1  
			je @@not_clear  ; if the next pixel is not black (0), don't move the asset and print the asset at the same loaction.
			inc dx
			loop @@check_right
		
		jmp @@clear

	
	@@not_clear:
		stc
		popa
		pop bp
		ret 6
	
	@@clear:
		clc 
		popa
		pop bp
		ret 6


endp check_collision
	
proc get_pixel_ahead  ; returns a color of a pixel in a given direction in pixel color.

	@@POSX EQU [word ptr bp+8]
	@@POSY EQU [word ptr bp+6]
	@@DIRECTION EQU [bp+4]

	push bp
	mov bp, sp
	pusha
	mov dx, @@DIRECTION  

    cmp dx, 'U'
    je @@up
	cmp dx, 'D'
    je @@down
	cmp dx, 'L'
    je @@left
	cmp dx, 'R'
    je @@right

	@@up:
		dec @@POSY
		jmp @@done
	@@down:
		mov bx, @@POSY
		add bx, 12
		mov @@POSY, bx
		jmp @@done
	@@left:
		dec @@POSX
		jmp @@done
	@@right:
		mov bx, @@POSX
		add bx, 12
		mov @@POSX, bx
		jmp @@done
	
	@@done:
		mov ah, 0Dh
		mov cx, @@POSX
		mov dx, @@POSY
		int 10h
		xor ah, ah
		mov @@POSX, ax ; mov the result to @@posx to later return th

	popa
	pop bp
	ret 4
endp get_pixel_ahead

proc print_message

	@@MESSAGE_POINTER EQU [bp + 8]
	@@CURSOR_POSX EQU [bp + 6]
	@@CURSOT_POSY EQU [bp + 4]

	push bp
	mov bp, sp
	pusha

	mov ax, 13h
    int 10h   ; Clear the screen.

	mov ah, 02
	xor bh, bh
	mov dl, @@CURSOR_POSX
	mov dh, @@CURSOT_POSY
	int 10h ; set cursor position

	mov dx, @@MESSAGE_POINTER
	mov ah, 9
	int 21h ; print game over.

	@@exit:
	popa
	pop bp
	ret 6
endp print_message

; pacman and ghosts

proc update_buffer

	@@USER_INPUT EQU [byte ptr bp + 4]

	push bp
	mov bp, sp
	pusha 
	
	mov cx, buffer_size - 1  
	lea bx, [input_buffer]  ; mov the adress of the buffer to bx
	add bx, buffer_size - 2

	xor ah, ah
	next_element:  ; move all of numbers in the buffer one index up (to make space for the user input)
		mov al, [byte ptr bx]
		mov [byte ptr bx + 1], al 
		dec bx

		loop next_element

	inc bx
	mov al, @@USER_INPUT
	mov [byte ptr bx], al  ; puts the user input in the first cell of the buffer.

	
	popa
	pop bp
	ret 2
endp update_buffer

proc check_buffer
	pusha 

	lea bx, [input_buffer]
	mov cx, buffer_size
	scan_buffer:  ; check whether all of the numbers in the buffer are the user input (the user holds the button)
		cmp [byte ptr bx], 0
		jne @@found_direction

		inc bx
		loop scan_buffer

	@@no_direction_found:  ; if not all equal - set carry flag.
		stc
		jmp @@done

	@@found_direction:  ; if equal - clear carry flag.
		clc
		xor ah, ah
		mov al, [byte ptr bx]
		mov [prev_direction], ax

	@@done:
	popa
	ret
endp check_buffer

proc move_pacman

	pusha

	mov ah, 1
	int 16h
	jz not_pressed  ; check if any key is pressed
	mov ah, 0
	int 16h  ; If a key is pressed, check which one (Result in al).

	cmp al, 27  ; check if the user had pressed the ESC key.
	jne @@continue
	jmp exit
	@@continue:
	
	cmp ah, 048h
	je @@change_up   ; check if keys other then the arrows are pressed.
	cmp ah, 050h
	je @@change_down
	cmp ah, 04Bh
	je @@change_left
	cmp ah, 04Dh
	je @@change_right
	jne print_pacman   ;  If none of the keys are pressed, move pacman at his current direction / don't move.

	not_pressed:
		push 0
		call update_buffer   ; enter 0 if no key was pressed.

		call check_buffer
		jc print_pacman  ; if buffer is empty, jump to print pacman and continue in the same direction.
		jmp check_buffer_direction  ; if buffer is not empty, check if the direction is valid. (previous direction in prev_direcion)

	change_direction:

		@@change_up:
			mov ax, 'U'
			mov dx, offset pacman_art_up
			mov [pacman_style], dx
			jmp @@update_buffer

		@@change_down:
			mov ax, 'D'
			mov dx, offset pacman_art_down
			mov [pacman_style], dx
			jmp @@update_buffer

		@@change_left:
			mov ax, 'L'
			mov dx, offset pacman_art_left
			mov [pacman_style], dx
			jmp @@update_buffer

		@@change_right:
			mov ax, 'R'
			mov dx, offset pacman_art_right
			mov [pacman_style], dx
			jmp @@update_buffer

		@@update_buffer:
		push ax
		call update_buffer  ; enter direction to buffer

		mov [prev_direction], ax   ; mov direction to this variabe to later check for validity.
	
	check_buffer_direction:
		push [pacman_posx]
		push [pacman_posy]
		push [prev_direction]
		call check_collision   ; check whether the buffered direction / user input is valid.
		jc print_pacman  ; if not continue in the same direction.

		mov ax, [prev_direction]  ; if direction is valid, change the current direction to the buffered direction / user input.
		mov [direction], ax


	print_pacman:

		push offset pacman_posx
		push offset pacman_posy
		call teleport_sprite

		push [pacman_posx]
		push [pacman_posy]
		push [direction]
		call check_collision  ; check if the current direction is valid.
		jc print_stationary
	
		push [pacman_style]
		push offset pacman_posx
		push offset pacman_posy
		push [direction]
		call move_asset	 ; summon the move_asset procedure to move pacman at the currect direction.
		jc @@exit

	print_stationary:
		push [pacman_style]
		push 12
		push 12
		push [pacman_posx]
		push [pacman_posy]
		call print_asset

	@@exit:
	popa
	ret
endp move_pacman

proc calcualte_distance

	@@ASSET_POSX EQU [bp + 10]
	@@ASSET_POSY EQU [bp + 8]
	@@TARGET_POSX EQU [bp + 6]
	@@TARGET_POSY EQU [bp + 4]

	@@DELTA_X EQU [bp - 2]
	@@DELTA_Y EQU [bp - 4]

	push bp
	mov bp, sp
	sub sp, 4
	pusha

	mov cx, @@ASSET_POSX ; calculate the delta of the x positions.
	mov ax, @@TARGET_POSX

	sub ax, cx ; find the absulute value of the number.
	cwd
	xor ax, dx
	sub ax, dx
	mov @@DELTA_X, ax

	mov dx, @@ASSET_POSY
	mov bx, @@TARGET_POSY ; calculate the delta of the x positions.

	sub bx, dx  ; find the absulute value of the number.
	mov ax, bx
	cwd
	xor ax, dx
	sub ax, dx
	mov @@DELTA_Y, ax

	mov ax, @@DELTA_X
	add ax, @@DELTA_Y  ; add the delta x and the delta y.

	mov @@ASSET_POSX, ax ; return the calculated value to the main program.

	popa
	add sp, 4
	pop bp
	ret 6
endp calcualte_distance

proc get_direction_to_target

	@@ASSET_POSX EQU [word ptr bp + 12]  ; parameters.
	@@ASSET_POSY EQU [bp + 10]
	@@TARGET_POSX EQU [bp + 8]
	@@TARGET_POSY EQU [bp + 6]
	@@CURR_DIRECTION EQU [word ptr bp + 4]

	@@SHORTEST_DISTANCE_INDEX EQU [word ptr bp - 2]  ; local variable.
		
	push bp
	mov bp, sp
	sub sp, 2
	pusha 

	@@up:
		push @@ASSET_POSX
		push @@ASSET_POSY
		push 'U'
		call check_collision ; check if the up direction is clear.
		jnc @@check_current_direction_up
		push 0FFFFh  ; if the up direction isn't clear, push ffff to the stack.
		jmp @@left  ; continue to the next direction.

		@@check_current_direction_up: 
		cmp @@CURR_DIRECTION, 'D'  ; check if the ghost is moving at the opposite direction (ghosts cannot turn 180 degrees)
		jne @@continue_up
		push 0FFFFh
		jmp @@left

		@@continue_up:
		mov cx, @@ASSET_POSX
		add cx, 6
		mov dx, @@ASSET_POSY
		dec dx
		mov ax, @@TARGET_POSX
		add ax, 6
		mov bx, @@TARGET_POSY
		add bx, 6   ; adjust the positions to the center of the ghost and target.

		push cx
		push dx
		push ax
		push bx
		call calcualte_distance  ; calculate the distance from the position of the ghost to the target.
		pop ax

		push ax  ; push the calculated distance to the stack.

	@@left:
		push @@ASSET_POSX
		push @@ASSET_POSY
		push 'L'
		call check_collision
		jnc @@check_current_direction_left
		push 0FFFFh
		jmp @@down

		@@check_current_direction_left:
		cmp @@CURR_DIRECTION, 'R'
		jne @@continue_left
		push 0FFFFh
		jmp @@down

		@@continue_left:
		mov cx, @@ASSET_POSX
		dec cx
		mov dx, @@ASSET_POSY
		add dx, 6
		mov ax, @@TARGET_POSX
		add ax, 6
		mov bx, @@TARGET_POSY
		add bx, 6   ; adjust the positions to the center of the asset.

		push cx
		push dx
		push ax
		push bx
		call calcualte_distance
		pop ax

		push ax
		
	@@down:
		push @@ASSET_POSX
		push @@ASSET_POSY
		push 'D'
		call check_collision
		jnc @@check_current_direction_down
		push 0FFFFh
		jmp @@right

		@@check_current_direction_down:
		cmp @@CURR_DIRECTION, 'U'
		jne @@continue_down
		push 0FFFFh
		jmp @@right

		@@continue_down:
		mov cx, @@ASSET_POSX
		add cx, 6
		mov dx, @@ASSET_POSY
		add dx, 12
		mov ax, @@TARGET_POSX
		add ax, 6
		mov bx, @@TARGET_POSY
		add bx, 6   ; adjust the positions to the center of the asset.

		push cx
		push dx
		push ax
		push bx
		call calcualte_distance
		pop ax

		push ax
		
	
	@@right:
		push @@ASSET_POSX
		push @@ASSET_POSY
		push 'R'
		call check_collision
		jnc @@check_current_direction_right
		push 0FFFFh
		jmp @@find_shortest_distance

		@@check_current_direction_right:
		cmp @@CURR_DIRECTION, 'L'
		jne @@continue_right
		push 0FFFFh
		jmp @@find_shortest_distance

		@@continue_right:
		mov cx, @@ASSET_POSX
		add cx, 12
		mov dx, @@ASSET_POSY
		add dx, 6
		mov ax, @@TARGET_POSX
		add ax, 6
		mov bx, @@TARGET_POSY
		add bx, 6   ; adjust the positions to the center of the asset.

		push cx
		push dx
		push ax
		push bx
		call calcualte_distance
		pop ax

		push ax


	@@find_shortest_distance:
		mov cx, 4 ; setup the needed registers for finding the smallest distance.
		mov bx, 0FFFFh
		@@next_distance:	
			pop ax  ; get a distance from the stack.
			cmp ax, bx  ; compare it to bx (the previous smallest distance / initial number).
			jae @@check_if_equal  ; if the distance is bigger then bx, continue to the next distance.
			mov bx, ax ; if the given distanve is smaller then the current smallest distance, set the current smallest distance to the given direction.
			mov @@SHORTEST_DISTANCE_INDEX, cx  ; save the current value of cx (to later identify the actual direction)
			jmp @@continue

			@@check_if_equal:
				cmp bx, ax
				jne @@continue
				mov @@SHORTEST_DISTANCE_INDEX, cx  ; save the current value of cx (to later identify the actual direction)

			@@continue:
			loop @@next_distance

		cmp @@SHORTEST_DISTANCE_INDEX, 4 ; identify the actual direction and return it to the main program.
		jne @@check_left
		mov @@ASSET_POSX, 'R'
		jmp @@exit

		@@check_left:
		cmp @@SHORTEST_DISTANCE_INDEX, 3
		jne @@check_down
		mov @@ASSET_POSX, 'D'
		jmp @@exit

		@@check_down:
		cmp @@SHORTEST_DISTANCE_INDEX, 2
		jne @@check_up
		mov @@ASSET_POSX, 'L'
		jmp @@exit

		@@check_up:
		cmp @@SHORTEST_DISTANCE_INDEX, 1
		jne @@exit
		mov @@ASSET_POSX, 'U'

	@@exit:
	popa
	add sp, 2
	pop bp
	ret 8

endp get_direction_to_target

proc get_random_direction
	
	@@GHOST_POSX EQU [word ptr bp + 8]
	@@GHOST_POSY EQU [bp + 6]
	@@CURR_DIRECTION EQU [byte ptr bp + 4]

	push bp
	mov bp, sp
	pusha

	@@randomize:
	mov ax, 040h
	mov es, ax
	mov ax, [es:06Ch]
	and al, 00000011b ; randomize a number between 0 - 3.

	cmp al, 0 ; assign each random number to a direction.
	je @@up
	cmp al, 1
	je @@down
	cmp al, 2
	je @@left
	jne @@right

	@@up:

		cmp @@CURR_DIRECTION, 'D'
		je @@CONTINUE_IN_CURR_DIRECTION ; ensure the ghost isn't going to do a 180.

		push @@GHOST_POSX
		push @@GHOST_POSY
		push 'U'
		call check_collision
		jc @@CONTINUE_IN_CURR_DIRECTION ; check that the randomized direction is clear. if not, continue in the same direction.

		mov @@GHOST_POSX, 'U'
		jmp @@exit

	@@down:
		cmp @@CURR_DIRECTION, 'U'
		je @@CONTINUE_IN_CURR_DIRECTION ; ensure the ghost isn't going to do a 180.

		push @@GHOST_POSX
		push @@GHOST_POSY
		push 'D'
		call check_collision
		jc @@CONTINUE_IN_CURR_DIRECTION ; check that the randomized direction is clear. if not, continue in the same direction.

		mov @@GHOST_POSX, 'D'
		jmp @@exit

	@@left:
		cmp @@CURR_DIRECTION, 'R'
		je @@CONTINUE_IN_CURR_DIRECTION ; ensure the ghost isn't going to do a 180.

		push @@GHOST_POSX
		push @@GHOST_POSY
		push 'L'
		call check_collision
		jc @@CONTINUE_IN_CURR_DIRECTION ; check that the randomized direction is clear. if not, continue in the same direction.

		mov @@GHOST_POSX, 'L'
		jmp @@exit

	@@right:
		cmp @@CURR_DIRECTION, 'L'
		je @@CONTINUE_IN_CURR_DIRECTION ; ensure the ghost isn't going to do a 180.

		push @@GHOST_POSX
		push @@GHOST_POSY
		push 'R'
		call check_collision
		jc @@CONTINUE_IN_CURR_DIRECTION ; check that the randomized direction is clear. if not, continue in the same direction.

		mov @@GHOST_POSX, 'R'
		jmp @@exit

	@@CONTINUE_IN_CURR_DIRECTION:

		mov al, @@CURR_DIRECTION
		xor ah, ah
		mov @@GHOST_POSX, ax

	@@exit:
	popa
	pop bp
	ret 4
endp get_random_direction

proc move_blibky

	@@BLINKY_STYLE EQU [word ptr bp-2]

	push bp
	mov bp, sp
	sub sp, 2
	pusha

	mov al, [blinky_mode] 
	xor ah, ah
	push ax
	push [blinky_posx]
	push [blinky_posy]
	call get_ghost_mode ; get the correct mode from the procedure (based on parameters).
	pop ax

	mov [blinky_mode], al ; insert the return value fromt he procedure to the ghost's mode variable.
	
	cmp [blinky_mode], 'c' ; check which mode the ghost is corrently in and then print it and move it accordingly.
	je @@chase_mode
	cmp [blinky_mode], 's'
	je @@scatter_mode
	cmp [blinky_mode], 'f'
	je @@frightened_mode
	cmp [blinky_mode], 'e'
	je @@eaten_mode

	@@chase_mode:

		push [blinky_posx]
		push [blinky_posy]
		call is_touching_pacman
		jnc @@continue_chase

		call game_over

		@@continue_chase:
		push [blinky_posx]
		push [blinky_posy]
		push [pacman_posx]
		push [pacman_posy]
		push [blinky_direction] 
		call get_direction_to_target 

		mov @@BLINKY_STYLE, offset blinky_art
		jmp @@print_blinky
	
	@@scatter_mode:

		push [blinky_posx]
		push [blinky_posy]
		call is_touching_pacman
		jnc @@continue_scatter

		call game_over

		@@continue_scatter:
		push [blinky_posx]
		push [blinky_posy]
		push 150
		push 0
		push [blinky_direction] 
		call get_direction_to_target

		mov @@BLINKY_STYLE, offset blinky_art
		jmp @@print_blinky

	@@frightened_mode:
		push [blinky_posx]
		push [blinky_posy]
		push [blinky_direction]
		call get_random_direction
		mov @@BLINKY_STYLE, offset frightened_ghost_art
		jmp @@print_blinky

	@@eaten_mode:
		push [blinky_posx]
		push [blinky_posy]
		push 80
		push 67
		push [blinky_direction] 
		call get_direction_to_target
		mov @@BLINKY_STYLE, offset eaten_ghost_art
		jmp @@print_blinky

	@@print_blinky:

	push offset blinky_posx
	push offset blinky_posy
	call teleport_sprite ; teleport the ghost if it reached the teleporters.

	pop ax
	xor ah, ah
	mov [blinky_direction], ax

	push [blinky_posx]
	push [blinky_posy]
	push [blinky_direction]
	call check_collision
	jc @@exit

	push @@BLINKY_STYLE
	push offset blinky_posx
	push offset blinky_posy
	push [blinky_direction]
	call move_asset


	@@exit:
	popa
	add sp, 2
	pop bp
	ret
endp move_blibky

proc calculate_pinky_target

	push bp
	mov bp, sp
	pusha

	@@check_above:
		cmp [pinky_direction], 'U'
		jne @@check_down

		mov ax, [pacman_posx]
		mov [pinky_target_x], ax
		sub [pinky_target_x], 30

		mov ax, [pacman_posy]
		mov [pinky_target_y], ax
		sub [pinky_target_y], 30

		jmp @@exit

	@@check_down:
		cmp [pinky_direction], 'D'
		jne @@check_left

		mov ax, [pacman_posx]
		mov [pinky_target_x], ax

		mov ax, [pacman_posy]
		mov [pinky_target_y], ax
		add [pinky_target_y], 30

		jmp @@exit

	@@check_left:
		cmp [pinky_direction], 'L'
		jne @@check_right

		mov ax, [pacman_posx]
		mov [pinky_target_x], ax
		sub [pinky_target_x], 30

		mov ax, [pacman_posy]
		mov [pinky_target_y], ax

		jmp @@exit

	@@check_right:

		mov ax, [pacman_posx]
		mov [pinky_target_x], ax
		add [pinky_target_x], 30

		mov ax, [pacman_posy]
		mov [pinky_target_y], ax

		jmp @@exit

	@@exit:
		popa
		pop bp
	ret 
endp calculate_pinky_target

proc move_pinky

	@@PINKY_STYLE EQU [word ptr bp-2]

	push bp
	mov bp, sp
	sub sp, 2
	pusha

	mov al, [pinky_mode] 
	xor ah, ah
	push ax
	push [pinky_posx]
	push [pinky_posy]
	call get_ghost_mode
	pop ax

	mov [pinky_mode], al
	
	cmp [pinky_mode], 'c'
	je @@chase_mode
	cmp [pinky_mode], 's'
	je @@scatter_mode
	cmp [pinky_mode], 'f'
	je @@frightened_mode
	cmp [pinky_mode], 'e'
	je @@eaten_mode

	@@chase_mode:

		push [pinky_posx]
		push [pinky_posy]
		call is_touching_pacman
		jnc @@continue_chase

		call game_over

		@@continue_chase:
		call calculate_pinky_target
		push [pinky_posx]
		push [pinky_posy]
		push [pacman_posx]
		push [pacman_posy]
		push [pinky_direction] 
		call get_direction_to_target

		mov @@PINKY_STYLE, offset pinky_art
		jmp @@print_pinky
	
	@@scatter_mode:

		push [pinky_posx]
		push [pinky_posy]
		call is_touching_pacman
		jnc @@continue_scatter

		call game_over

		@@continue_scatter:
		push [pinky_posx]
		push [pinky_posy]
		push 4
		push 3
		push [pinky_direction] 
		call get_direction_to_target

		mov @@PINKY_STYLE, offset pinky_art
		jmp @@print_pinky

	@@frightened_mode:
		push [pinky_posx]
		push [pinky_posy]
		push [pinky_direction]
		call get_random_direction
		mov @@PINKY_STYLE, offset frightened_ghost_art
		jmp @@print_pinky

	@@eaten_mode:
		push [pinky_posx]
		push [pinky_posy]
		push 80
		push 67
		push [pinky_direction] 
		call get_direction_to_target
		mov @@PINKY_STYLE, offset eaten_ghost_art
		jmp @@print_pinky

	@@print_pinky:

	push offset pinky_posx
	push offset pinky_posy
	call teleport_sprite

	pop ax
	xor ah, ah
	mov [pinky_direction], ax

	push [pinky_posx]
	push [pinky_posy]
	push [pinky_direction]
	call check_collision
	jc @@exit

	push @@PINKY_STYLE
	push offset pinky_posx
	push offset pinky_posy
	push [pinky_direction]
	call move_asset


	@@exit:
	popa
	add sp, 2
	pop bp
	ret
endp move_pinky

proc calculate_clyde_target

	push [clyde_posx]
	push [clyde_posy]
	push [pacman_posx]
	push [pacman_posy]
	call calcualte_distance
	pop ax

	cmp ax, 45 ; check whther clyde is in near pacman (if clyde is near pacman he should target the botto left corner of the map).
	jg @@outside_bounds

	mov [clyde_target_x], 0
	mov [clyde_target_y], 195
	jmp @@exit

	@@outside_bounds:
		mov ax, [pacman_posx]
		mov [clyde_target_x], ax
		mov ax, [pacman_posy]
		mov [clyde_target_y], ax

	@@exit:
	ret
endp calculate_clyde_target

proc move_clyde

	@@CLYDE_STYLE EQU [word ptr bp-2]

	push bp
	mov bp, sp
	sub sp, 2
	pusha

	mov al, [clyde_mode] 
	xor ah, ah
	push ax
	push [clyde_posx]
	push [clyde_posy]
	call get_ghost_mode
	pop ax

	mov [clyde_mode], al
	
	cmp [clyde_mode], 'c'
	je @@chase_mode
	cmp [clyde_mode], 's'
	je @@scatter_mode
	cmp [clyde_mode], 'f'
	je @@frightened_mode
	cmp [clyde_mode], 'e'
	je @@eaten_mode

	@@chase_mode:

		push [clyde_posx]
		push [clyde_posy]
		call is_touching_pacman
		jnc @@continue_chase

		call game_over

		@@continue_chase:
		call calculate_clyde_target
		push [clyde_posx]
		push [clyde_posy]
		push [pacman_posx]
		push [pacman_posy]
		push [clyde_direction] 
		call get_direction_to_target

		mov @@CLYDE_STYLE, offset clyde_art
		jmp @@print_clyde
	
	@@scatter_mode:

		push [clyde_posx]
		push [clyde_posy]
		call is_touching_pacman
		jnc @@continue_scatter

		call game_over

		@@continue_scatter:
		push [clyde_posx]
		push [clyde_posy]
		push 20
		push 195
		push [clyde_direction] 
		call get_direction_to_target

		mov @@CLYDE_STYLE, offset clyde_art
		jmp @@print_clyde

	@@frightened_mode:
		push [clyde_posx]
		push [clyde_posy]
		push [clyde_direction]
		call get_random_direction
		mov @@CLYDE_STYLE, offset frightened_ghost_art
		jmp @@print_clyde

	@@eaten_mode:
		push [clyde_posx]
		push [clyde_posy]
		push 80
		push 67
		push [clyde_direction] 
		call get_direction_to_target
		mov @@CLYDE_STYLE, offset eaten_ghost_art
		jmp @@print_clyde

	@@print_clyde:

	push offset clyde_posx
	push offset clyde_posy
	call teleport_sprite

	pop ax
	xor ah, ah
	mov [clyde_direction], ax

	push [clyde_posx]
	push [clyde_posy]
	push [clyde_direction]
	call check_collision
	jc @@exit

	push @@CLYDE_STYLE
	push offset clyde_posx
	push offset clyde_posy
	push [clyde_direction]
	call move_asset


	@@exit:
	popa
	add sp, 2
	pop bp
	ret
endp move_clyde

proc calculate_inky_target

	push bp
	mov bp, sp
	pusha

	@@check_above:
		cmp [direction], 'U'
		jne @@check_down

		mov ax, [pacman_posx]
		mov [inky_target_x], ax
		sub [inky_target_x], 2

		mov ax, [pacman_posy]
		mov [inky_target_y], ax
		sub [inky_target_y], 10

		jmp @@continue

	@@check_down:
		cmp [direction], 'D'
		jne @@check_left

		mov ax, [pacman_posx]
		mov [inky_target_x], ax

		mov ax, [pacman_posy]
		mov [inky_target_y], ax
		add [inky_target_y], 10

		jmp @@continue

	@@check_left:
		cmp [direction], 'L'
		jne @@check_right

		mov ax, [pacman_posx]
		mov [inky_target_x], ax
		sub [inky_target_x], 10

		mov ax, [pacman_posy]
		mov [inky_target_y], ax

		jmp @@continue

	@@check_right:

		mov ax, [pacman_posx]
		mov [inky_target_x], ax
		add [inky_target_x], 10

		mov ax, [pacman_posy]
		mov [inky_target_y], ax

		jmp @@continue

	@@continue:

		mov ax, [blinky_posx]
		mov bx, [inky_target_x]
		sub ax, bx
		sub [inky_target_x], ax

		mov ax, [blinky_posy]
		mov bx, [inky_target_y]
		sub ax, bx
		sub [inky_target_y], ax
	@@exit:
	popa
	pop bp
	ret 
endp calculate_inky_target

proc move_inky

	@@INKY_STYLE EQU [word ptr bp-2]

	push bp
	mov bp, sp
	sub sp, 2
	pusha

	mov al, [inky_mode] 
	xor ah, ah
	push ax
	push [inky_posx]
	push [inky_posy]
	call get_ghost_mode
	pop ax

	mov [inky_mode], al
	
	cmp [inky_mode], 'c'
	je @@chase_mode
	cmp [inky_mode], 's'
	je @@scatter_mode
	cmp [inky_mode], 'f'
	je @@frightened_mode
	cmp [inky_mode], 'e'
	je @@eaten_mode

	@@chase_mode:

		push [inky_posx]
		push [inky_posy]
		call is_touching_pacman
		jnc @@continue_chase

		call game_over

		@@continue_chase:
		call calculate_inky_target
		push [inky_posx]
		push [inky_posy]
		push [pacman_posx]
		push [pacman_posy]
		push [inky_direction] 
		call get_direction_to_target

		mov @@INKY_STYLE, offset inky_art
		jmp @@print_inky
	
	@@scatter_mode:

		push [inky_posx]
		push [inky_posy]
		call is_touching_pacman
		jnc @@continue_scatter

		call game_over

		@@continue_scatter:
		push [inky_posx]
		push [inky_posy]
		push 195
		push 195
		push [inky_direction] 
		call get_direction_to_target

		mov @@INKY_STYLE, offset inky_art
		jmp @@print_inky

	@@frightened_mode:
		push [inky_posx]
		push [inky_posy]
		push [inky_direction]
		call get_random_direction
		mov @@INKY_STYLE, offset frightened_ghost_art
		jmp @@print_inky

	@@eaten_mode:
		push [inky_posx]
		push [inky_posy]
		push 80
		push 67
		push [inky_direction] 
		call get_direction_to_target
		mov @@INKY_STYLE, offset eaten_ghost_art
		jmp @@print_inky

	@@print_inky:

	push offset inky_posx
	push offset inky_posy
	call teleport_sprite

	pop ax
	xor ah, ah
	mov [inky_direction], ax

	push [inky_posx]
	push [inky_posy]
	push [inky_direction]
	call check_collision
	jc @@exit

	push @@INKY_STYLE
	push offset inky_posx
	push offset inky_posy
	push [inky_direction]
	call move_asset


	@@exit:
	popa
	add sp, 2
	pop bp
	ret
endp move_inky

proc is_touching_pacman
	
	@@GHOST_POSX EQU [bp + 6]
	@@GHOST_POSY EQU [bp + 4]

	@@DIRECTION EQU [bp - 2]
	@@INDEX EQU [word ptr bp - 4]

	push bp
	mov bp, sp
	sub sp, 4
	pusha

	clc  ; clears the carry flag (resets the collision indicator)
	mov @@INDEX, 1
	mov cx, 4
	@@next_direction:
		push cx

		lea bx, [directions]
		add bx, @@INDEX
		mov ax, [word ptr bx]
		xor ah, ah
		mov @@DIRECTION, ax  ; inserts the direction we want to check to the local variable.

		xor dx, dx
		mov cx, 12
		@@check_side:

			push cx
			cmp @@INDEX, 2
			ja @@check_horizontal

			mov bx, @@GHOST_POSX
			add bx, dx
			push bx ; send the current x postion to the procedure (move over the 12 different x positions).
			push @@GHOST_POSY ; send the current y postion to the procedure.
			jmp @@continue

			@@check_horizontal:
			push @@GHOST_POSX ; send the current x postion to the procedure.
			mov bx, @@GHOST_POSY
			add bx, dx
			push bx ; send the current y postion to the procedure (move over the 12 different x positions).
			jmp @@continue

			@@continue:
			push @@DIRECTION   ; push 'U' to the procedure (which sets the direction that the next procedure will check).
			call get_pixel_ahead  ; calls a procedure who puts the next pixel's color in the var pixel_color (to check whether the path is clear).
			pop ax
			cmp ax, 14
			je @@touching  ; if the next pixel is not black (0), don't move the asset and print the asset at the same loaction.
			inc dx
			pop cx
			loop @@check_side

		inc @@INDEX
		pop cx
		loop @@next_direction

	jmp @@not_touching

	@@touching:
		pop cx
		pop cx
		popa
		add sp, 4
		pop bp
		stc
		jmp @@exit
	@@not_touching:
		clc
		popa
		add sp, 4
		pop bp
	@@exit:
	ret 4
endp is_touching_pacman

proc get_ghost_mode

	@@CURR_MODE EQU [word ptr bp + 8]
	@@GHOST_POSX EQU [word ptr bp + 6]
	@@GHOST_POSY EQU [word ptr bp + 4]
	
	push bp
	mov bp, sp
	pusha 

	cmp @@CURR_MODE, 'e'
	je @@check_eaten_mode ; check which mode the ghost is currently in.

	cmp @@CURR_MODE, 'f' 
	je check_frightened_mode

	jmp change_mode_by_clock

	@@check_eaten_mode:	

		cmp @@GHOST_POSX, 82 ; check if the ghost reched the openining of the ghost house.
		jne @@exit

		cmp @@GHOST_POSY, 67
		jne @@exit

		jmp change_mode_by_clock ; if the ghost reached the opening of the ghost house, change the mode acording to the clock.

	check_frightened_mode:

		cmp [frightened_game_cycle], 1800 ; 1800 game cycles = ~ 10 seconds (at cycles = max)
		ja change_mode_by_clock ; if the frightened period ended, return to normal modes (chase / scatter).

		inc [frightened_game_cycle] ; increse the frightened cycle.
		mov @@CURR_MODE, 'f'

		push @@GHOST_POSX
		push @@GHOST_POSY 
		call is_touching_pacman ; check if the ghost is touching pacman (return in the carry flag).
		jnc @@exit

		add [points], 200 ; if the pacman touched the ghost, add 200 points and change to eaten mode.

		mov @@CURR_MODE, 'e' 

		push offset black_square
		push 12
		push 12
		push @@GHOST_POSX
		push @@GHOST_POSY
		call print_asset ; draw a blck square around the ghost to delete it (becasue the eaten mode aray contains transparent pixel
						 ; which in some cases won't delete the whole ghost).

		jmp @@exit

	change_mode_by_clock:
		push @@CURR_MODE
		call get_mode_by_clock ; get a mode from the procedure and return it.
		pop @@CURR_MODE

	@@exit:
	popa	
	pop bp
	ret 4
endp get_ghost_mode

proc get_mode_by_clock

	CURR_MODE EQU [word ptr bp + 4]

	push bp
	mov bp, sp
	pusha 

	mov al, [current_mode_period]
	cmp [clock], al ; compare the current value of the clock to when it should switch modes.
	jne @@return_mode ; if the program doesn't have to switch modes, return the current mode.

	inc [mode_cycle] ; if the program needs to switch mode, increse the mode cycle and update the next goal.
	mov bx, offset ghost_mode_periods
	add bl, [mode_cycle]
	mov al, [byte ptr bx]
	mov [current_mode_period], al

	mov [clock], 0 ; reset the clock.

	@@return_mode:
	mov al, [mode_cycle]
	xor ah, ah
	mov bl, 2
	div bl ; calculate the returned mode by checking wether the mode cycle is even / odd. (even = chase, odd = scatter).

	cmp ah, 0
	je return_chase

	mov CURR_MODE, 's'
	jmp @@exit

	return_chase:
	mov CURR_MODE, 'c'

	@@exit:
	popa
	pop bp
	ret
endp get_mode_by_clock

proc teleport_sprite

	SPRITE_POSX_POINTER EQU [word ptr bp + 6]
	SPRITE_POSY_POINTER EQU [word ptr bp + 4]

	push bp
	mov bp, sp
	pusha

	mov bx, SPRITE_POSY_POINTER
	cmp [word ptr bx], 85 ; check wether the sprite is at the correct y position.
	jne @@exit

	mov bx, SPRITE_POSX_POINTER
	cmp [word ptr bx], 165 ; check whether the sprite entered the teleporter from the right.
	je @@right_teleporter

	mov bx, SPRITE_POSX_POINTER
	cmp [word ptr bx], 0 ; check whether the sprite entered the teleporter from the left.
	je @@left_teleporter

	jmp @@exit

	@@right_teleporter:
	push offset black_square
	push 12
	push 12
	mov bx, SPRITE_POSX_POINTER
	push [word ptr bx]
	mov bx, SPRITE_POSY_POINTER
	push [word ptr bx]
	call print_asset ; delete the sprite from the current location.

	mov bx, SPRITE_POSX_POINTER
	mov [word ptr bx], 0 ; change the sprite's position variables to move it the the left side of the map.
	jmp @@exit

	@@left_teleporter:
	push offset black_square
	push 12
	push 12
	mov bx, SPRITE_POSX_POINTER
	push [word ptr bx]
	mov bx, SPRITE_POSY_POINTER
	push [word ptr bx]
	call print_asset ; delete the sprite from the current location.

	mov bx, SPRITE_POSX_POINTER
	mov [word ptr bx], 165 ; change the sprite's position variables to move it the the left side of the map.


	@@exit:
	popa
	pop bp
	ret 4
endp teleport_sprite

; points and power pellets 

proc refresh_points

	pusha 

	push offset points_cords
	push NUMBER_OF_POINTS
	call update_cords_array ; call the function that updates the array of cordinates (to earase dots which pacman had eaten)

	jnc @@continue ; check if the procedure returned a true (in form of setting the carry flag) - meaning an item has been deleted.
	add [points], 10
	inc [point_counter]

	@@continue:
	mov bx, offset points_cords
	mov cx, NUMBER_OF_POINTS
	@@print_point:
		cmp [word ptr bx], 0FFFFh ; if one of the values in the array is FFFF - skip it (meaning pacman has eaten this dot)
		je @@next_point

		push offset point_art
		push 12
		push 12
		push [word ptr bx]
		push [word ptr bx + 2]
		call print_asset ; print each dot in the array at the correct cordinates.

		@@next_point:
		add bx, 4
		loop @@print_point

	@@exit:
	popa
	ret 
endp refresh_points

proc update_cords_array

	@@ARRAY_POINTER EQU [bp + 6]
	@@NUMBER_OF_ITEMS EQU [bp + 4]

	push bp
	mov bp, sp
	pusha

	mov bx, @@ARRAY_POINTER
	mov cx, @@NUMBER_OF_ITEMS
	@@next_item:
		push cx
		mov ax, [pacman_posx]
		cmp ax, [word ptr bx] ; check if the x position of the item lines up with the one of pacman.
		jne @@continue ; if not - continue to the next item.

		mov ax, [pacman_posy]
		cmp ax, [word ptr bx + 2] ; check if the y position of the item lines up with the one of pacman.
		jne @@continue ; if not - continue to the next item.

		push bx
		mov [word ptr bx], 0FFFFh ; if the cordinates of the item lines up with the position of pacman - 
		add bx, 2 ; earase them from the array (move FFFF into the X and Y of the item)
		mov [word ptr bx], 0FFFFh
		pop bx

		stc ; set the carry flag to 1 (to inform the main program that an item has been deleted)
		pop cx
		jmp @@exit ; exit the procedure (pacman can't be on two points at the same time).

		@@continue:
		add bx, 4 ; increment the pointer of the array to the next item.
		pop cx
		loop @@next_item

	@@exit: 
	popa
	pop bp
	ret 4
endp update_cords_array

proc print_point_count

	@@NUMBER_OF_DIGITS EQU [word ptr bp-2]

	push bp
	mov bp, sp
	sub sp, 2
	pusha

	mov @@NUMBER_OF_DIGITS, 5

	mov ax, [points]
	mov cx, @@NUMBER_OF_DIGITS
	@@next_digit:
		xor dx, dx 
		mov bx, 10
		div bx ; div the number with 10 (to seperate the digits). 
		push dx ; push the digit to the stack.
		loop @@next_digit

	mov ah, 02
	xor bh, bh
	mov dh, 0
	mov dl, 62
	int 10h ; set the cursor to the wanted positions.

	mov dx, offset point_string
	mov ah, 9
	int 21h ; print the "points: " string to the screen.

	mov cx, @@NUMBER_OF_DIGITS
	@@print_digit:
		mov @@NUMBER_OF_DIGITS, cx
		mov ah, 02
		pop dx ; print each digit by poping it from the stack.
		add dl, 30h
		int 21h

		mov cx, @@NUMBER_OF_DIGITS
		loop @@print_digit

	popa
	add sp, 2
	pop bp
	ret
endp print_point_count

proc check_if_won

	cmp [point_counter], NUMBER_OF_POINTS ; check if the points counter reached the number on points (player finished the game).
	jne @@exit

	call won_game

	@@exit:
	ret
endp check_if_won

proc refresh_power_pellets
	pusha 

	push offset power_pellet_cords
	push NUMBER_OF_POWER_pelletS
	call update_cords_array ; call the function that updates the array of cordinates (to earase dots which pacman had eaten)

	jnc @@continue
	mov [frightened_game_cycle], 0
	mov [blinky_mode], 'f'
	mov [pinky_mode], 'f'
	mov [clyde_mode], 'f'
	mov [inky_mode], 'f'

	@@continue:
	mov bx, offset power_pellet_cords
	mov cx, NUMBER_OF_POWER_pelletS
	@@print_power_pellet:
		cmp [word ptr bx], 0FFFFh ; if one of the values in the array is FFFF - skip it (meaning pacman has eaten this dot)
		je @@next_power_pellet

		push offset power_pellet_art
		push 12
		push 12
		push [word ptr bx]
		push [word ptr bx + 2]
		call print_asset ; print each dot in the array at the correct cordinates.

		@@next_power_pellet:
		add bx, 4
		loop @@print_power_pellet

	popa
	ret 
endp refresh_power_pellets

proc print_lives

	pusha

	cmp [pacman_lives], 0
	je @@exit

	mov ax, 180
	mov cl, [pacman_lives]
	xor ch, ch
	next_print:
		push offset pacman_art_right
		push 12
		push 12
		push ax
		push 183
		call print_asset

		add ax, 14
		loop next_print

	@@exit:
	popa
	ret
endp print_lives

proc game_over

	pusha

	cmp [pacman_lives], 0
	je exit_game

	push offset respawning_string
	push 8
	push 10
	call print_message

	mov ah, 0
	int 16h

	dec [pacman_lives]
	call reset_game
	jmp @@continue_game

	exit_game:
	push offset game_over_string
	push 15
	push 11
	call print_message

	mov ah, 0
	int 16h ; wait for a key to close the program

	jmp exit

	@@continue_game:
	popa
	pop ax ; I need to even out the stack to continue the game so i'm poping the return adress (which is not needed) to ax.
	jmp start_game

	@@exit:
	popa
	ret
endp game_over

proc won_game

	pusha

	push offset winning_string
	push 15
	push 11
	call print_message

	mov ah, 0
	int 16h ; wait for a key to close the program

	jmp exit

	@@exit:
	popa
	ret
endp won_game

; clock and timing

proc make_delay

	pusha

	mov ah, 86h
	mov cx, 0
	mov dx, 03200h
	int 15h   ; make a delay of ~ 14000 microseconds 

	@@exit:
	popa
	ret
endp make_delay

proc update_clock

	pusha

	mov ah, 2Ch
	int 21h

	cmp [clock_save], dh ; check if there was a change in the clock.
	je @@exit

	mov [clock_save], dh ; if there was a change in the clock savem, incrament the clock and reset the save.
	inc [clock]

	cmp [clock], 60 ; if the clock reached 60, reset it to 0.
	jne @@exit

	mov [clock], 0

	@@exit:
	popa
	ret 
endp update_clock

; setup and game reset

proc initiate_game

	pusha 

	mov cx, 5
	xor ax, ax
	xor bx, bx
	@@draw_map_part:		
		push ax
		push 195
		push 35
		push bx
		push 0
		call print_asset

		add ax, 01AA9h ; add the size of one section of the map (ro print the next one)
		add bx, 35
		loop @@draw_map_part

	mov ah, 2Ch
	int 21h
	mov [clock_save], dh

	@@exit:
	popa
	ret
endp initiate_game

proc reset_game

	pusha

	mov [blinky_posx], 82 ; reset sprit positions.
	mov [blinky_posy], 69
	mov [pinky_posx], 3
	mov [pinky_posy], 4
	mov [clyde_posx], 30
	mov [clyde_posy], 179
	mov [inky_posx], 147
	mov [inky_posy], 145

	mov [pacman_posx], 82
	mov [pacman_posy], 104

	mov [direction], ? ; reset sprit directions.
	mov [prev_direction], ?
	mov [blinky_direction], ?
	mov [pinky_direction], ?
	mov [clyde_direction], ?
	mov [inky_direction], ?

	mov [frightened_game_cycle], 0 ; reset clock and timing variables.
	mov [mode_cycle], 1
	mov [current_mode_period], 7
	mov [clock], 1

	mov [blinky_mode], 's'
	mov [pinky_mode], 's'
	mov [clyde_mode], 's'
	mov [inky_mode], 's'

	@@exit:
	popa
	ret
endp reset_game

start:
	mov ax, @data
    mov ds, ax  

	mov ax, 0a00h
    mov es, ax

	start_game:

	mov ax, 13h
    int 10h   ; Initiate video mode.

	call initiate_game
	call print_lives

	game_loop:
		call update_clock
		call refresh_points
		call refresh_power_pellets

		call move_pacman
		call move_blibky
		call move_pinky
		call move_clyde
		call move_inky

		call print_point_count
		call check_if_won

		call make_delay
		jmp game_loop


exit:
	mov ax, 2  
	int 10h  ; Switch to text mode.

	mov ax, 4c00h
	int 21h
END start






	
	
	


