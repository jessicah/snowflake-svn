(* FreeDesktop Clock -- O'Caml *)

let pi = 2. *. asin 1.;;

module FDHand = struct
	let pi_6 = pi /. 6.
	let pi_2 = pi /. 2.
	let sqrt2 = sqrt 2.
	
	let draw_hour cr width length =
		let r = width /. 2. in
		Cairo.move_to cr length (-.r);
		Cairo.arc cr length 0. r (-.pi_2) (pi_2);
		Cairo.line_to cr (width *. sqrt2) r;
		Cairo.arc cr 0. 0. (r *. 2.) pi_6 (-.pi_6);
		Cairo.close_path cr
	
	let draw_minute cr width length =
		let r = width /. 2. in
		Cairo.move_to cr length (-.r);
		Cairo.arc cr length 0. r (-.pi_2) (pi_2);
		Cairo.line_to cr 0. r;
		Cairo.line_to cr 0. (-.r);
		Cairo.close_path cr
	
	let draw_second cr width length =
		let r = width /. 2. in
		let thick = width in
		let back = length /. 3. in
		let back_thin = length /. 10. in
		Cairo.move_to cr length (-.r);
		Cairo.arc cr length 0. r (-.pi_2) (pi_2);
		Cairo.line_to cr (-.back_thin) r;
		Cairo.line_to cr (-.back_thin) thick;
		Cairo.line_to cr (-.back) (-.thick);
		Cairo.line_to cr (-.back_thin) (-.thick);
		Cairo.line_to cr (-.back_thin) (-.r);
		Cairo.close_path cr
	
	let draw_hand cr noon_deg width length alt draw =
		let m = Cairo.get_matrix cr in
			Cairo.translate cr (alt /. 2.) alt;
			Cairo.rotate cr (noon_deg *. pi /. 180. -. pi_2);
			draw cr width length;
		Cairo.set_matrix cr m
	
	let hour_width = 0.03
	let hour_length = 0.25
	let hour_alt = 0.010
	
	let minute_width = 0.015
	let minute_length = 0.39
	let minute_alt = 0.020
	
	let second_width = 0.0075
	let second_length = 0.32
	let second_alt = 0.026
	
	let draw_time cr width height draw_seconds =
		let (hours,minutes,seconds) = Time.time () in
		
		let second_angle = float seconds *. 6. in
		let minute_angle = float minutes *. 6. +. second_angle /. 60. in
		let hour_angle = float hours *. 30. +. minute_angle /. 12. in
		
		Cairo.save cr;
			Cairo.scale cr width height;
			Cairo.translate cr 0.5 0.5;
			draw_hand cr hour_angle hour_width hour_length hour_alt draw_hour;
			draw_hand cr minute_angle minute_width minute_length minute_alt draw_minute;
			if draw_seconds then
				draw_hand cr second_angle second_width second_length second_alt draw_second;
			Cairo.set_source_rgba cr 0. 0. 0. 0.5;
			Cairo.fill cr;
			draw_hand cr hour_angle hour_width hour_length 0. draw_hour;
			draw_hand cr minute_angle minute_width minute_length 0.0 draw_minute;
			if draw_seconds then
				draw_hand cr second_angle second_width second_length 0. draw_second;
			Cairo.set_source_rgb cr 1. 1. 1.;
			Cairo.fill cr;
		Cairo.restore cr
	
	let draw_now cr width height seconds =
		draw_time cr width height seconds
end;;

module FDLogo = struct
	let draw_boundary cr =
		Cairo.move_to cr 63. 36.;
		Cairo.curve_to cr 63. 43. 58. 47. 51. 47.;
		Cairo.line_to cr 13. 47.;
		Cairo.curve_to cr 6. 47. 1. 43. 1. 36.;
		Cairo.line_to cr 1. 12.;
		Cairo.curve_to cr 1. 5. 6. 1. 13. 1.;
		Cairo.line_to cr 51. 1.;
		Cairo.curve_to cr 58. 1. 63. 5. 63. 12.;
		Cairo.close_path cr
	
	let draw_outline cr =
		Cairo.set_source_rgb cr 1. 1. 1.;
		Cairo.set_line_width cr 2.;
		draw_boundary cr;
		Cairo.stroke cr
	
	let draw_background cr =
		Cairo.save cr;
			Cairo.set_source_rgb cr 1.0 0.6 0.0;
			Cairo.translate cr 3.5 3.5;
			Cairo.scale cr 0.887 0.848;
			draw_boundary cr;
			Cairo.fill cr;
		Cairo.restore cr
	
	let draw_window cr =
		Cairo.move_to cr (-6.) (-7.125);
		Cairo.line_to cr 6. (-7.125);
		Cairo.curve_to cr 8. (-7.125) 9. (-6.125) 9. (-4.125);
		Cairo.line_to cr 9. 4.125;
		Cairo.curve_to cr 9. 6.125 8. 7.125 6. 7.125;
		Cairo.line_to cr (-6.) 7.125;
		Cairo.curve_to cr (-8.) 7.125 (-9.) 6.125 (-9.) 4.125;
		Cairo.line_to cr (-9.) (-4.125);
		Cairo.curve_to cr (-9.) (-6.125) (-8.) (-7.125) (-6.) (-7.125);
		Cairo.close_path cr
	
	let draw_window_at cr x y scale =
		Cairo.save cr;
			Cairo.translate cr x y;
			Cairo.scale cr scale scale;
			draw_window cr;
			Cairo.save cr;
				Cairo.set_source_rgb cr 1. 1. 1.;
				Cairo.fill cr;
			Cairo.restore cr;
			Cairo.set_source_rgb cr 0.231 0.502 0.682;
			Cairo.scale cr (1. /. scale) (1. /. scale);
			Cairo.stroke cr;
		Cairo.restore cr
	
	let draw_windows cr =
		Cairo.save cr;
			Cairo.move_to cr 18. 16.125;
			Cairo.line_to cr 48.25 20.375;
			Cairo.line_to cr 30.25 35.825;
			Cairo.close_path cr;
			Cairo.set_source_rgba cr 1. 1. 1. 0.5;
			Cairo.stroke cr;
		Cairo.restore cr;
		draw_window_at cr 18. 16.126 1.;
		draw_window_at cr 48.25 20.375 0.8;
		draw_window_at cr 30.25 35.825 0.5
	
	let rot_x_factor = 1.086
	let rot_y_factor = 1.213
	let _width = 64. *. rot_x_factor
	let _height = 48. *. rot_y_factor
	
	let draw cr width height =
		Cairo.save cr;
			let x_scale = width /. _width
			and y_scale = height /. _height in
			let scale = if x_scale < y_scale then x_scale else y_scale in
			let x_off = (width -. (scale *. _width)) /. 2.
			and y_off = (height -. (scale *. _height)) /. 2. in
			Cairo.translate cr x_off y_off;
			Cairo.scale cr scale scale;
			Cairo.translate cr (-2.5) 14.75;
			Cairo.rotate cr (-0.274990703529840);
			draw_outline cr;
			draw_background cr;
			draw_windows cr;
		Cairo.restore cr
end;;

module FDFace = struct
	let draw_fancy_tick cr radius =
		Cairo.save cr;
			(*Cairo.arc cr 0. 0. radius 0. (2. *. pi);
			Cairo.set_source_rgb cr 1. 1. 1.;
			Cairo.fill cr;*)
			Cairo.set_source_rgb cr 1. 1. 1.;
			Cairo.set_line_width cr (radius *. 2. /. 3.);
			Cairo.arc cr 0. 0. (radius *. 2.) 0. (2. *. pi);
			Cairo.stroke cr;
		Cairo.restore cr
	
	let draw_plain_tick cr radius =
		Cairo.save cr;
			Cairo.arc cr 0. 0. radius 0. (2. *. pi);
			Cairo.set_source_rgb cr 1. 1. 1.;
			Cairo.fill cr;
		Cairo.restore cr
	
	let draw cr width height =
		Cairo.save cr;
			Cairo.scale cr width height;
			Cairo.save cr;
				Cairo.translate cr 0.15 0.15;
				Cairo.scale cr 0.7 0.7;
				FDLogo.draw cr 1. 1.;
			Cairo.restore cr;
			Cairo.translate cr 0.5 0.5;
			Cairo.scale cr 0.93 0.93;
			for i = 0 to 59 do
				let minute = float_of_int i in
				Cairo.save cr;
					let radians = minute *. 6. *. pi /. 180. in
					Cairo.rotate cr radians;
					Cairo.translate cr 0. 0.5;
					if i mod 15 = 0 then draw_fancy_tick cr 0.015
					else if i mod 5 = 0 then draw_fancy_tick cr 0.01
					else draw_plain_tick cr 0.01;
				Cairo.restore cr;
			done;
		Cairo.restore cr
end

let clock_width = 200.
let clock_height = 200.

let clear cr width height alpha =
	Cairo.save cr;
	Cairo.set_source_rgba cr 1. 1. 1. alpha;
	Cairo.set_operator cr Cairo.OPERATOR_SOURCE;
	Cairo.rectangle cr 0. 0. width height;
	Cairo.fill cr;
	Cairo.restore cr

let make_background display_cr width height =
	let back_surface = Cairo.image_surface_create Cairo.FORMAT_RGB24 width height in
	let cr = Cairo.create back_surface in
	
	clear cr (float_of_int width) (float_of_int height) 1.;
	
	Cairo.set_source_surface cr (Cairo.get_target display_cr) 0. 0.;
	Cairo.paint cr;
	
	FDFace.draw cr (float width) (float height);
	
	(*Cairo.destroy cr;*)
	back_surface

let main display_cr x_position y_position =
	let width = clock_width and height = clock_height in
	let u_width = int_of_float width and u_height = int_of_float height in
	
	let background = make_background display_cr u_width u_height in
	let buffer_surface = Cairo.image_surface_create Cairo.FORMAT_RGB24 u_width u_height in
	let cr = Cairo.create buffer_surface in
	Cairo.rectangle display_cr x_position y_position width height;
	Cairo.clip display_cr;
	let thread_fun = (fun () -> while true do
		Cairo.set_source_surface cr background 0. 0.;
		Cairo.paint cr;
		FDHand.draw_now cr width height true;
		Cairo.set_source_surface display_cr buffer_surface x_position y_position;
		Cairo.paint display_cr;
		(*Thread.delay 0.5;*)
		Thread.yield ();
	done) in
	ignore (Thread.create thread_fun () "fdclock")

