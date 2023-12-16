% ============= funkcjonalne=====================
:- (dynamic i_am_at/1, at/2, have/1, path/3, pickable/1, locked/1, progress_point/1, talkable/1, talked_to/1, inspectable/1, already_been_in/1, bridge_broke_down/0, crawlable/1, opened_vent_cover/0).
:- retractall(at(_, _)),
   retractall(i_am_at(_)),
   retractall(alive(_)).
:- (discontiguous i_am_at/1, at/2, have/1, path/3, pickable/1, describe/1, inspectable/1, inspect/1, locked/1, open/1, use/2, progress_point/1, talkable/1, talked_to/1, type_code/2).

progress_point(intro).


use(_, Y):-
    i_am_at(Place),
    \+ at(Y, Place),
    write("There is no "), write(Y), write(" here."), !, nl.
    
inspect(X):-
    i_am_at(Place),
    \+ at(X, Place),
    nl, write("There is no "), write(X), write(" here."), !, nl.


inspect(X):-
    i_am_at(Place),
    \+ at(X, Place),
    nl, write("There is no "), write(X), write(" here."), !, nl.
    
    
    

take(X) :-
    have(X),
    nl,
    write('You\'re already holding it!'), !,
    nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    pickable(X),
    retract(at(X, Place)),
    assert(have(X)),
    nl,
    write('You took the '),
    write(X),
    write('.'),
    nl, !,
    nl.

take(X) :-
    i_am_at(Place),
    at(X, Place),
    nl,
    write('You can\'t pick up the '),
    write(X),
    write('.'),
    nl, !,
    nl.

take(_) :-
    nl,
    write('I don\'t see it here.'),
    nl.


/* These rules describe how to put down an object. */
drop(X) :-
    have(X),
    i_am_at(Place),
    retract(have(X)),
    assert(at(X, Place)),
    nl,
    write('You dropped the '),
    write(X),
    write('.'),
    nl, !,
    nl.

drop(_) :-
    nl,
    write('You aren\'t have it!'),
    nl.

% shortcuts for go
n :-
    go(n).

s :-
    go(s).

e :-
    go(e).

w :-
    go(w).


%  This rule tells how to move in a given direction.
go(Direction) :-
    i_am_at(Here),
    path(Here, Direction, There),
    retract(i_am_at(Here)),
    assert(i_am_at(There)), !,
    nl,
    write("You went "),
    nl,
    look.

go(_) :-
    nl,
    write("You can't go that way.").

% look logic
look :-
    i_am_at(Place),
    describe(Place),
    nl,
    notice_objects_at(Place), !.


% notice objects at places
notice_objects_at(Place) :-
    at(X, Place),
    write("There is one *"),
    write(X),
    write("* here."),
    nl,
    fail.

notice_objects_at(_).


check_inventory() :-
    have(X),
    nl,
    write("You have one *"),
    write(X),
    write("* in your bag."),
    nl,
    fail.

check_inventory(_).


/* This rule tells how to die. */
die :-
    finish.


finish :-
    nl,
    write('The game is over. Please enter the "halt." command.'),
    nl.

/* This rule just writes out game instructions. */
instructions :-
    nl,
    write('Enter commands using standard Prolog syntax.'),
    nl,
    write('Available commands are:'),
    nl,
    write('start.             -- to start the game.'),
    nl,
    write('n.  s.  e.  w.     -- to go in that direction.'),
    nl,
    write('take(Object).      -- to pick up an object.'),
    nl,
    write('drop(Object).      -- to put down an object.'),
    nl,
    write('use(Object1, Object2). -- to use an Object1 on Object2.'),
    nl,
    write('open(Object).      -- to open Object if it\'s possible.'),
    nl,
    write('type_code(Object, code) -- to enter a 4 digit code into an Object'),
    nl,
    write('kick(Object)       -- to kick an Object'),
    nl,
    write('check_inventory.   -- to list the objects you are holding.'),
    nl,
    write('inspect(Object).   -- to inspect an object.'),
    nl,
    write('look.              -- to look around you again.'),
    nl,
    write('instructions.      -- to see this message again.'),
    nl,
    write('halt.              -- to end the game and quit.'),
    nl,
    nl.

start :-
    instructions,
    look.





% ===============================================





% =================jedrek===================================
% My location
i_am_at(crew_bedroom).

% Descriptions

% crew_bedroom
describe(crew_bedroom) :-
    progress_point(intro),
    nl,
    write("A loud crashing sound wakes you up in you bed inside the engineering crew bedroom. You look around and see that the room is in a mess. A vent in the east corner of the room swings wide open. Blaring alarms can be heard from the main corridor on the south side. You see a locker, a desk and a bed. You need to act, fast."),
    nl,
    nl,
    write("You are at the crew bedroom."),
    nl,
    nl,
    retract(progress_point(intro)),
    assert(progress_point(main_c)), !.

describe(crew_bedroom) :-
    nl,
    write("You are in the crew bedroom, there is a security door on the south, and a vent entrance on the east."),
    nl, !,
    nl.

% crew_bedroom_vent
describe(crew_bedroom_vent) :-
    nl,
    write("You crawl into a rather spatious crew bedroom vent. There it is! This is where you *desk_key* went! Good thing it didn't fall deeper or you would be stuck in here for ever."),
    nl, !,
    nl.

% main_corridor
describe(main_corridor) :-
    progress_point(main_c),
    nl,
    write("You decide it's finally time to leave your quarters. Staying here definetely won't help you find out what's going on. "),
    nl,
    write("You enter the main corridor, but you can barely see anything. You can see the reason now why the alarms are going off. "),
    nl,
    write("A fire is raging only a few meters in front of you. 'Probably just an overcharged e-box' - your engineer's instinct tells you. However you still can't pass through it. You need to find a way to put it out."),
    nl,
    nl,
    write("Behind your back, at the west end, there is a supply cabinet, but the east end of the corridor lays behind the fire. "),
    nl,
    retract(progress_point(main_c)),
    assert(progress_point(todo)), !,
    nl.

describe(main_corridor) :-
    nl,
    write("You are in the main corridor of the living section."),
    nl, !,
    nl.




% Crafting
craft(hammer_head, hammer_handle) :-
    have(hammer_head),
    have(hammer_handle),
    retract(have(hammer_head)),
    retract(have(hammer_handle)),
    assert(have(hammer)),
    nl,
    write("Those two came togheter easily! Now you've got a full *hammer*!"),
    nl, !,
    nl.

craft(hammer_handle, hammer_head) :-
    have(hammer_head),
    have(hammer_handle),
    retract(have(hammer_head)),
    retract(have(hammer_handle)),
    assert(have(hammer)),
    nl,
    write("Those two came togheter easily! Now you've got a full *hammer*!"),
    nl, !,
    nl.

craft(cyber_key_handle, cyber_key_shaft, cyber_key_head) :-
    have(cyber_key_handle),
    have(cyber_key_shaft),
    have(cyber_key_head),
    retract(have(cyber_key_handle)),
    retract(have(cyber_key_shaft)),
    retract(have(cyber_key_head)),
    assert(have(cyber_key)),
    nl,
    write("You put the three pieces of the cyber key togheter and they magnetically snap into place."),
    nl,
    write("Now you've got a full *cyber_key*!"),
    nl, !,
    nl.

craft(cyber_key_shead, cyber_key_shaft, cyber_key_handle) :-
    have(cyber_key_handle),
    have(cyber_key_shaft),
    have(cyber_key_head),
    retract(have(cyber_key_handle)),
    retract(have(cyber_key_shaft)),
    retract(have(cyber_key_head)),
    assert(have(cyber_key)),
    nl,
    write("You put the three pieces of the cyber key togheter and they magnetically snap into place."),
    nl,
    write("Now you've got a full *cyber_key*!"),
    nl, !,
    nl.

craft(cyber_key_head, cyber_key_shaft, cyber_key_handle) :-
    have(cyber_key_handle),
    have(cyber_key_shaft),
    have(cyber_key_head),
    nl,
    write("Hmm, seems almost right, maybe in a diffrent order?"),
    nl, !,
    nl.

craft(cyber_key_head, cyber_key_handle, cyber_key_shaft) :-
    have(cyber_key_handle),
    have(cyber_key_shaft),
    have(cyber_key_head),
    nl,
    write("Hmm, seems almost right, maybe in a diffrent order?"),
    nl, !,
    nl.

craft(cyber_key_shaft, cyber_key_head, cyber_key_handle) :-
    have(cyber_key_handle),
    have(cyber_key_shaft),
    have(cyber_key_head),
    nl,
    write("Hmm, seems almost right, maybe in a diffrent order?"),
    nl, !,
    nl.


craft(cyber_key_handle, cyber_key_head, cyber_key_shaft) :-
    have(cyber_key_handle),
    have(cyber_key_shaft),
    have(cyber_key_head),
    nl,
    write("Hmm, seems almost right, maybe in a diffrent order?"),
    nl, !,
    nl.



% Crew Bedroom ================================================================

% Paths at crew_bedroom
% crew_bedroom n<=>s main_corridor
% has to be unlocked

% crew_bedroom w<=>e crew_bedroom_vent
path(crew_bedroom, w, crew_bedroom_vent).
path(crew_bedroom_vent, e, crew_bedroom).

% Objects at crew_bedroom
at(bed, crew_bedroom).
inspectable(bed).

at(desk, crew_bedroom).
locked(desk).
inspectable(desk).

at(locker, crew_bedroom).
inspectable(locker).

at(security_door, crew_bedroom).
locked(security_door).
inspectable(security_door).

% Use cases - Crew Bedroom
use(crew_access_card, security_door) :-
    have(crew_access_card),
    locked(security_door),
    retract(locked(security_door)),
    nl,
    write("You swipe your crew access card on the security door and hear the metal latch inside loudly open. Good thing the electronics on the ship still work."),
    nl, !,
    nl.

use(crew_access_card, security_door) :-
    have(crew_access_card),
    nl,
    write("The security door is already unlocked."),
    nl, !,
    nl.

open(security_door) :-
    locked(security_door),
    nl,
    write("The security door is locked. It won't budge."),
    nl, !,
    nl.

open(security_door) :-
    nl,
    write("The security door, now unlocked, opens smoothly. In front of you, you see the main corridor shrouded in smoke."),
    nl,
    assert(path(crew_bedroom, s, main_corridor)),
    assert(path(main_corridor, n, crew_bedroom)), !,
    nl.

use(desk_key, desk) :-
    have(desk_key),
    locked(desk),
    retract(locked(desk)),
    retract(at(desk, crew_bedroom)),
    assert(at(crew_access_card, crew_bedroom)),
    assert(pickable(crew_access_card)),
    nl,
    write("You unlock the desk with the key you found in the vent. Inside you see your *crew_access_card*."),
    nl, !,
    nl.


% Inspects - Crew Bedroom
inspect(bed):-
    inspectable(bed),
    nl, write("There is nothing special on this bed, aside from a *thick_blanket*. I wonder if it could be useful later."), nl,
    assert(at(thick_blanket, crew_bedroom)),
    assert(pickable(thick_blanket)),
    retract(at(bed, crew_bedroom)),
    retract(inspectable(bed)),
    !, nl.

inspect(desk) :-
    locked(desk),
    nl,
    write("The desk is locked. There should be a key on it, but maybe it flew somewhere during the crash."),
    nl, !,
    nl.

inspect(locker) :-
    inspectable(locker),
    nl,
    write("You look inside your locked hoping to find some of your tools. "),
    nl,
    write("Unfortunately, all of the electronic tools got damaged. However your trusty hammer only came apart. I bet you could stick the *hammer_head* and the *hammer_handle* togheter and fix it up."),
    assert(at(hammer_head, crew_bedroom)),
    assert(at(hammer_handle, crew_bedroom)),
    assert(pickable(hammer_head)),
    assert(pickable(hammer_handle)),
    retract(at(locker, crew_bedroom)),
    retract(inspectable(locker)),
    !, nl.

inspect(security_door) :-
    locked(security_door),
    nl,
    write("The door leading out of your room to the main corridor remains locked. You need your own *crew_access_card* to unlock it."),
    nl, !,
    nl.

inspect(security_door) :-
    nl,
    write("The door leading out of your room to the main corridor is unlocked. You can now go to the main corridor."),
    nl, !,
    nl.

% ====================================================

% Crew Bedroom Vent
at(desk_key, crew_bedroom_vent).
pickable(desk_key).

at(space_latch, crew_bedroom_vent).
inspectable(space_latch).

% Paths at crew_bedroom_vent
path(void, w, crew_bedroom_vent).


% Inspects - Crew Bedroom Vent
inspect(space_latch) :-
    have(space_suit),
    inspectable(space_latch),
    nl, write("With a *space_suit* you can exit through the *space_latch* and traverse from one point on the ship to another quickly, while avoiding a lot of obstacles!"), nl,
    !, nl.

inspect(space_latch):-
    inspectable(space_latch),
    nl, write("This space latch is a way outside the ship. However, going through it without proper equipment may end very poorly for you."), nl, !, nl.

open(space_latch) :-
    have(space_suit),
    nl,
    write("Suited up, you open the space latch, you can now go outside to the void. However if you do, you better find another latch quickly."),
    nl,
    assert(path(crew_bedroom_vent, w, void)), !,
    nl.

open(space_latch) :-
    nl,
    write("You open the space latch and enter the void outside of the ship. Going out there without proper equipment was not your brightest idea... You feel the air being sucked out of your lungs. You die."),
    nl,
    die, !,
    nl.

% ====================================================

% Main Corridor
at(flaming_electric_box, main_corridor).
inspectable(flaming_electric_box).

at(supply_cabinet, main_corridor).
locked(supply_cabinet).
inspectable(supply_cabinet).


extend_env_main_c :-
    % Create the reset of the corridor
    assert(at(south_corridor_exit_door, main_corridor)),
    % assert(locked(south_corridor_exit_door)),
    assert(inspectable(south_corridor_exit_door)),

    assert(at(wounded_engineering_chief, main_corridor)),
    assert(inspectable(wounded_engineering_chief)),

    assert(at(cantine_entrance_door, main_corridor)),
    assert(inspectable(cantine_entrance_door)),

    nl, write("After the fire went down and the smoke cleared out a little bit, the rest of the corrdior becomes visibile, but you will look around later."), nl,
    write("Finally you see someone alive! It\'s , Qaux\'ods, *wounded_engineering_chief* from the planet Luzxore."), nl,
    write("He is hurt, but he looks like he is trying to tell you something."),
    nl, !,
    nl.


% Use cases - Main Corridor %


% Electric box
inspect(flaming_electric_box):-
    inspectable(flaming_electric_box),
    nl, write("The electric box is on fire. You need to put it out somehow."), nl, !, nl.

inspect(electric_box) :-
    nl,
    write("The e-box is not burining anymore. The atmosphere has become much more pleasant."),
    nl, !,
    nl.

use(thick_blanket, flaming_electric_box) :-
    have(thick_blanket),
    retract(at(flaming_electric_box, main_corridor)),
    assert(at(electric_box, main_corridor)),
    nl,
    write("Why didn't you think about that earlier! Of course, suppressing the fire with this thick wooly blanket put it down."),
    nl,
    nl,
    write("The fire is down and you can finally go through, but your blanket is burnt to a crisp."),
    nl,
    retract(have(thick_blanket)),
    retract(inspectable(flaming_electric_box)),
    extend_env_main_c,
    !, nl.



% Supply cabinet
inspect(supply_cabinet):-
    inspectable(supply_cabinet),
    locked(supply_cabinet),
    nl,
    write("The supply cabinet is wrapped in a chain and locked with a padlock. There is now way there is a key here."),
    nl, !,
    nl.

inspect(supply_cabinet):-
    inspectable(supply_cabinet),
    nl, write("Inside a heap of junk you find a *right_space_suit_glove* and a *space_suit_jacket*. Those will definetly be useful."), nl,
    write("There is also a *universal_speech_translator* here. It will come in handy if you encounter other crew members... or aliens."), nl,
    assert(at(right_space_suit_glove, main_corridor)),
    assert(pickable(right_space_suit_glove)),

    assert(at(space_suit_jacket, main_corridor)),
    assert(pickable(space_suit_jacket)),
    assert(at(universal_speech_translator, main_corridor)),
    assert(pickable(universal_speech_translator)),

    retract(at(supply_cabinet, main_corridor)),

    retract(inspectable(supply_cabinet)), !, nl.


use(hammer, supply_cabinet) :-
    nl,
    write("Ah yes, brute force. Always a good solution."),
    nl,
    nl,
    write("After smashing the cabinet open with a hammer, you can look for anything useful that you can find inside."),
    nl,
    retract(locked(supply_cabinet)),
    nl, !,
    nl.



% Wounded engineering chief
inspect(wounded_engineering_chief):-
    inspectable(wounded_engineering_chief),
    nl, write("This is the chief of your engineering crew, Qaux\'ods from the planet Luzxore. He is wounded, but he looks like he is trying to tell you something."), nl,
    !, nl.


talk(wounded_engineering_chief) :-
    talked_to(wounded_engineering_chief),
    talkable(wounded_engineering_chief),
    nl,
    write("Qaux'ods: I've already told you all I could. Now go on young one, survive. There is nothing you can do for me now."),
    nl, !,
    nl.

talk(wounded_engineering_chief) :-
    talkable(wounded_engineering_chief),
    nl,
    write("You: Hey, chief, are you okay? What happened?"),
    nl,
    nl,
    write("Qaux'ods: I'm not sure. I was in the cantine, eating grapes at table 9, when the ship started shaking. I ran out to see what's going on and I saw a bright flash of light."),
    nl,
    write("The captain told me through the radio (plot_element), but as soon as he started explaining, the radio went silent."),
    nl,
    write("I ran to the main corridor to see if I can help anyone, but I was hit by a piece of debris. I'm not sure how long I can hold on."),
    nl,
    nl,
    write("You: I'm sure I can help you somehow! We can get out of here togheter!"),
    nl,
    nl,
    write("Qaux'ods: No, I'm afraid it's too late for me. You need to go on and survive. The ship took a heavy blow, it won't hold on for long."),
    nl,
    write("Take my access card, it will open up my office, it will help you get to the escape pod. Get there and escape."),
    nl,
    write("My cyber-key was shatterd when I fell in the initial impact, but here's a piece, maybe you can find the rest of it."),
    nl,
    nl,
    write("You: Thank you, chief. I will never forget what you've done for me."),
    nl,
    nl,
    write("Qaux'ods: It's nothing but my duty. Now go on, I need to rest."),
    nl,
    nl,
    assert(have(engineering_chief_access_card)),
    assert(have(cyber_key_handle)),
    assert(talked_to(wounded_engineering_chief)),
    write("An *engineering_chief_access_card* and a *cyber_key_handle* were added into your inventory."), !,
    nl,
    nl.

talk(wounded_engineering_chief) :-
    nl,
    write("You: Hey, chief, are you okay? What happened?"),
    nl,
    nl,
    write("Qaux'ods: ⟟⏁'⌇ ⊬⍜⎍ ⎎⟟⋏⏃⌰⌰⊬! ⌇⍜⋔⟒⏁⊑⟟⋏☌ ⊑⏃⌿⌿⟒⋏⎅! ⌰⟒⏁ ⋔⟒ ⊑⟒⌰⌿ ⊬⍜⎍!!!"),
    nl,
    write("You: I can't understand anything. God, if I only knew Luzxorian..."),
    nl, !,
    nl.


use(universal_speech_translator, wounded_engineering_chief) :-
    talkable(wounded_engineering_chief),
    have(universal_speech_translator),
    nl,
    write("It seems like your speech translator already picked up on Luzxorian. You should already be able to talk to the chief."), !,
    nl.

use(universal_speech_translator, wounded_engineering_chief) :-
    have(universal_speech_translator),
    nl,
    write("You point the pointy end of the translator at Qaux'ods and press the button. The translator starts to hum and glow."),
    nl,
    write("It seems like it's working, maybe now you can talk to him."),
    nl,
    assert(talkable(wounded_engineering_chief)), !,
    nl.



% Cantine entrance 

inspect(cantine_entrance_door):-
    inspectable(cantine_entrance_door),
    talked_to(wounded_engineering_chief),
    nl,
    write("This is the entrance to the cantine on the far east side. That's where Quax'ods fell. I better go there."),
    nl, !,
    nl.

inspect(cantine_entrance_door):-
    inspectable(cantine_entrance_door),
    nl, write("This door leads to the cantine on the far east side. It\'s not locked."), nl,
    !, nl.

open(cantine_entrance_door) :-
    nl,
    write("The door swung open freely, you can now enter the cantine."),
    nl,
    assert(path(main_corridor, e, cantine)),
    assert(path(cantine, w, main_corridor)), !,
    nl.


% South corridor exit door

% inspect(south_corridor_exit_door):-
%     inspectable(south_corridor_exit_door),
%     locked(south_corridor_exit_door),
%     nl, write("This exit leads out of the living space to the (plot_element) section of the ship."), nl,
%     write("If you want to go further, you need to find out a way to unlock it with something."), nl,
%     !, nl.

inspect(south_corridor_exit_door):-
    inspectable(south_corridor_exit_door),
    nl, write("This exit leads out of the living space to the (plot_element) section of the ship."), nl,
    write("The door is unlocked, you can go through it if you open it."), nl,
    !, nl.

use(engineering_chief_access_card, south_corridor_exit_door) :-
    have(engineering_chief_access_card),
    nl,
    write("This will not work. This card is not useful for a door like that. It needs a proper key, not a higher access level."),
    nl, !,
    nl.

% use(cyber_key, south_corridor_exit_door) :-
%     have(cyber_key),
%     nl,
%     write("You insert the cyber key into the door and it turns with a loud hum and click. "),
%     nl,
%     write("The door is finally open, and you can enter the (plot_element) section of the ship."),
%     nl,
%     assert(path(main_corridor, s, engine_room)),
%     assert(path(engine_room, n, main_corridor)),
%     retract(locked(south_corridor_exit_door)),
%     !,
%     nl.



open(south_corridor_exit_door) :-
    assert(path(main_corridor, s, engine_room)),
    assert(path(engine_room, n, main_corridor)),
    write("The doors swung right open, they lead to the (plot_element_ID1), through the engine room, this is probably your only route to get out of here."), !,
    nl.
    



% The Cantine %
at(table_21, cantine).
inspectable(table_21).
at(table_8, cantine).
inspectable(table_8).
at(table_5, cantine).
inspectable(table_5).
at(table_12, cantine).
inspectable(table_12).
at(table_91, cantine).
inspectable(table_91).
at(table_9, cantine).
inspectable(table_9).
at(table_1, cantine).
inspectable(table_1).
at(table_34, cantine).
inspectable(table_34).

at(locked_safety_box, cantine).
inspectable(locked_safety_box).
locked(locked_safety_box).

% Use cases - Cantine %

% Tables
inspect(table_21):-
    inspectable(table_21),
    nl, write("You find some powdered scrambled eggs and a burnt toast. But nothing useful."), nl, !, nl.

inspect(table_8):-
    inspectable(table_8),
    nl, write("At this table there is some spilled gravy and a bowl of mashed potatos. Nothing useful though."), nl, !, nl.

inspect(table_5):-
    inspectable(table_5),
    nl, write("This table is empty. There is nothing here."), nl, !, nl.

inspect(table_12):-
    inspectable(table_12),
    nl, write("This table is empty. There is nothing here."), nl, !, nl.

inspect(table_91):-
    inspectable(table_91),
    nl, write("On this table there is nothing but little squared carrots and peas. Someone\'s a picky eater."), nl, !, nl.

inspect(table_9):-
    inspectable(table_9),
    nl, write("On this table you find some half-eaten grapes."), nl,
    write("After searching on the ground, you see a *cyber_key_shaft* laying there."), nl,
    assert(at(cyber_key_shaft, cantine)),
    assert(pickable(cyber_key_shaft)),
    retract(inspectable(table_9)),
    !, nl.

inspect(table_1):-
    inspectable(table_1),
    nl, write("Half of an apple juice box, spilled across the table, nothing more."), nl, !, nl.

inspect(table_34):-
    inspectable(table_34),
    nl, write("This table is empty. There is nothing here."), nl, !, nl.


% Safety box

inspect(locked_safety_box):-
    inspectable(locked_safety_box),
    locked(locked_safety_box),
    nl,
    write("You see a *locked_safety_box*, with wires on the control panel ripped apart."),
    nl,
    write("It won't open right now, but maybe if you reconnect the wires you could open it."),
    nl,
    nl,
    write("There are 3 sockets and 3 cables. You need to connect them in the right order."),
    nl,
    write(" Blue - b"),
    nl,
    write(" Red - r"),
    nl,
    write(" Yellow - y"),
    nl,
    nl,
    write("Use the command box_wiring(cable1, cable2, cable3). to connect the wires."),
    nl, !,
    nl.

box_wiring(y, b, r) :-
    locked(locked_safety_box),
    retract(locked(locked_safety_box)),
    retract(at(locked_safety_box, cantine)),
    assert(at(open_safety_box, cantine)),
    nl,
    write("That's it! The wires crackled and the box opened! You should see what's inside."),
    nl, !,
    nl.

box_wiring(_, _, _) :-
    locked(locked_safety_box),
    nl,
    write("Hmmm, that doesn't seem to be working.' Try again."),
    nl, !,
    nl.

box_wiring(_, _, _) :-
    nl,
    write("The box is already open."),
    nl, !,
    nl.

inspect(open_safety_box) :-
    nl,
    write("Inside the box you find a *cyber_key_head*! It payed off to fiddle with the cables."),
    nl,
    assert(at(cyber_key_head, cantine)),
    assert(pickable(cyber_key_head)), !,
    nl.




% =================jedrek===================================



% =================olek===================================

describe(engine_room) :-
    \+ already_been_in(engine_room),
    write("Damn, ship must have taken a really heavy blow, these engines look like they will explode in any second now."),
    nl,
    write("Security protocol must have kicked in, because the bridge to the (plot_element_ID1) is lifted to the ceeling, you need to find some way to lower it down."),
    nl,
    write("Maybe this *control_panel* might help."),
    assert(already_been_in(engine_room)),
    assert(at(control_panel, engine_room)), !,
    nl.

describe(engine_room) :-
    bridge_broke_down,
    write("After the bridge felt down, it left *bridge_gap* and opened entrance to the *nearby_vent*, that looks like it could be crawled into."),
    nl, nl, write("Hint: You can use crawl(Somwhere), to crawl inside of somehere!"), !,
    nl.

describe(engine_room) :-
    write("Ladder is covering the gap in the bridge, so soth to cross it."), !,
    nl.

inspect(control_panel) :-
    write("It seems like the control panel requires some kind of key to use It."), !,
    nl.

use(cyber_key, control_panel) :-
    have(cyber_key),
    i_am_at(engine_room),
    retract(have(cyber_key)),
    retract(at(control_panel, engine_room)),
    write("The key seemes to fit perfectly, you hear a loud noise, and the bridge to the (plot_element_ID1) starts lowering down..."),
    nl,
    write("It is making a loud noise, engine room must have really taken a lot of damage."),
    nl,
    write("SNAP! The bridge broke down, and fell to the bottom. On its way it made a hole in nearby vent."),
    nl,
    assert(at(nearby_vent, engine_room)),
    assert(at(bridge_gap, engine_room)),
    assert(crawlable(nearby_vent)),
    assert(bridge_broke_down),
    look, !,
    nl.

path(nearby_vent, e, vent_dead_end).
path(vent_dead_end, w, nearby_vent).
path(nearby_vent, w, vent_exit).
path(vent_exit, e, nearby_vent).

crawl(nearby_vent) :-
    i_am_at(engine_room),
    crawlable(nearby_vent),
    retract(i_am_at(engine_room)),
    assert(i_am_at(nearby_vent)),
    write("You crawled into the vent, there are two directions. One leads east and the other west."), !,
    nl.

crawl(service_room_vent) :-
    i_am_at(service_room),
    retract(i_am_at(service_room)),
    assert(i_am_at(vent_exit)),
    write("You crawled back into the vent."), !,
    nl.

describe(vent_dead_end) :-
    write("Vent gets too narrow in here, I should probably try the other way."), !,
    nl.

describe(nearby_vent) :-
    write("I can *exit_vent*, or go only in two directions from here, weast and east."), !,
    nl.

exit_vent :-
    i_am_at(nearby_vent),
    retract(i_am_at(nearby_vent)),
    assert(i_am_at(engine_room)),
    write("You crawled back into the engine room."), !,
    nl.

describe(vent_exit) :-
    opened_vent_cover,
    write("You crawled to the vent cover that you kick opened."), !,
    nl.

describe(vent_exit) :-
    write("You crawled to the *vent_cover*, check whats behind it."), !,
    nl.

inspect(vent_cover) :-
    i_am_at(vent_exit),
    write("It seems to be mounted preety rigidly, but maybe with some good kick I will be able to open it."), nl, nl,
    write("Hint: write kick(Object). in order to kick an object."), !,
    nl.

open(vent_cover) :-
    i_am_at(vent_exit),
    write("It seems to be mounted preety rigidly, but maybe with some good kick I will be able to open it."),  !,
    nl.

at(window, service_room).
at(locker_1, service_room).
at(locker_2, service_room).
at(locker_3, service_room).
at(locked_crate, service_room).
at(service_room_doors, service_room).
at(service_room_vent, service_room).
at(ladder, void).
pickable(ladder).

kick(vent_cover) :-
    i_am_at(vent_exit),
    write("You kicked the vent cover with all your might, and it fell down. The futher path west is no longer obstructed."),
    assert(path(vent_exit, w, service_room)),
    assert(opened_vent_cover), !,
    nl.

describe(service_room) :-
    write("You are in the service room, It was used by ships technicians."), !,
    nl.

inspect(window) :-
    i_am_at(service_room),
    write("You look through the window, and you see the *ladder* laying just outside the space latch. It might be of some use to you."), !,
    nl.

inspect(locker_1) :-
    i_am_at(service_room),
    write("Just some junk... but wait there is some *schematic* laying here as well."),
    assert(at(schematic, service_room)),
    retract(at(locker_1, service_room)), !,
    nl.

inspect(schematic) :-
    i_am_at(service_room),
    write("Space suit assebly guide:"),
    nl,
    write("Required parts: *space_suit_trousers*, *space_suit_jacket*, *space_suit_gloves*, *space_suit_helmet*."), !,
    nl.

inspect(locker_2) :-
    i_am_at(service_room),
    write("Nothing in here. Some nuts and bolts..."),
    retract(at(locker_2, service_room)), !,
    nl.

inspect(locker_3) :-
    i_am_at(service_room),
    write("Huh?, a *uv_flashlight*, maybe It will be of some use."),
    assert(at(uv_flashlight, service_room)),
    assert(pickable(uv_flashlight)),
    retract(at(locker_3, service_room)), !,
    nl.

inspect(locked_crate) :-
    i_am_at(service_room),
    write("It has an electronic lock on, that required 4 digits. You can try to guess it, but without some info it will can be a tedious task. `type_code(locked_create, *your code*)`"), !,
    nl.

type_code(locked_crate, 9911) :-
    i_am_at(service_room),
    write("Nice!. The code worked. Inside lays a *space_suit_trousers* and *space_suit_helmet*."),
    assert(at(space_suit_trousers, service_room)),
    assert(pickable(space_suit_trousers)),
    assert(at(space_suit_helmet, service_room)),
    assert(pickable(space_suit_helmet)),
    retract(at(locked_crate, service_room)), !,
    nl.

type_code(locked_crate, _) :-
    i_am_at(service_room),
    write("Wrong code."), !,
    nl.

use(uv_flashlight, locked_crate) :-
    i_am_at(service_room),
    at(locked_crate, service_room),
    have(uv_flashlight),
    write("You shine the flashlight on the crate, and you see some fingerprints on the 9 and 1 buttons."), !,
    nl.

use(ladder, bridge_gap) :-
    i_am_at(engine_room),
    have(ladder),
    retract(have(ladder)),
    retract(at(bridge_gap, engine_room)),
    retract(bridge_broke_down),
    write("You put the ladder in the gap, and you can now cross the bridge to the east."),
    assert(path(engine_room, e, workshop)),
    !,
    nl.
% are you taking a whole ladder through the vents???? - M %

craft(space_suit_trousers, space_suit_jacket, space_suit_gloves, space_suit_helmet) :-
    have(space_suit_jacket),
    have(space_suit_trousers),
    have(space_suit_gloves),
    have(space_suit_helmet),
    retract(have(space_suit_jacket)),
    retract(have(space_suit_trousers)),
    retract(have(space_suit_gloves)),
    retract(have(space_suit_helmet)),
    assert(have(space_suit)),
    write("You put all the pieces togheter, and you have a full *space_suit*. Now it is safe for you to walk in the outer space."), !,
    nl.

describe(void) :-
    write("From the outside you can really see the scale of the destruction, something must have hit the ship really hard."), !,
    nl.

% =================olek===================================




% =================michal===================================
/* starts in workshop. */
/* There is an entrance to the engineering chief's office where the player finds passcode to activating the escape pods,
an alien mass blocking the way to the escape pods, a fire in the corner,  a broken table,
the player goes into the last sector,
Objectives:, 
1. destroy window to suck the alien out
OR use fire to burn away alien mass blocking the path.

 3. fix broken escape pod control console
 4. Escape by going into escape pod and entering the code


 Workshop:
    - 
    - 
    - Toolbox (closed, unlocked):
        - Electrical tools
    - black alien mass
    - Window
    -

You have to be able to:
- inspect everything
- use the statue to break the window (alien, statue and window disappear, make path to pods)
- use the hammer on the window with no effect but a message DONE
- use the saw on the table (get wooden table leg, destroy table, break saw) DONE
- use the wood on the fire (get burning wood)  DONE
- use the burning wood on the alien mass (both disappear, creates path to escape pods) DONE
- use electrical tools on broken console 
- type in launch code to escape pod console
 */
path(workshop, w, engine_room).
at(black_sludge, workshop).
at(engineering_chief_office_door, workshop).
locked(engineering_chief_office_door).
at(toolbox, workshop).
at(workshop_window, workshop).
at(small_fire, workshop).
at(table, workshop).


describe(workshop) :-

    write("The workshop is where most engineering on the station happens."),
    write("It's dark, with the ocasional sparks flying out from damaged equipment"),
    nl,
    write("There is a door leading north into the engineering chief's office and a path south."),
    nl,
    (at(black_sludge, workshop); at(alien_mass, workshop)),
    write("A black mass is blocking the path south"),
    nl.

% describing specific elements in the workshop
inspect(black_sludge) :- 
    write("A strange black mass near the *workshop_window* blocks the path south. It pulsates slightly, as if breathing."),
    nl,
    write("Underneath it you see one of your collegues being slowly absorbed by what you assume to be some kind of alien intruder."),
    nl,
    write("A familiar smell of fuel fumes seems to be eminating from the creature."),
    nl,
    retract(at(black_sludge, workshop)),
    assert(at(alien_mass, workshop)),
    !.

inspect(alien_mass) :-
    write("The mass blocking the path next to the *workshop_window* pulsates slowly."), nl,
    write("You still feel the fumes similar to rocket fuel. It migth be flammable"), nl,
    !.

inspect(engineering_chief_office_door) :-
    locked(engineering_chief_office_door),
    write("The door to the chief's office, locked by an access card"),
    nl, !.

inspect(engineering_chief_office_door) :-
    write("The door to the chief's office, now open"), nl, !.

inspect(toolbox):- 
    i_am_at(workshop),
    write("Standard-issue toolbox. Some tools seem to be missing but you see some *electrical_tools* and a *hand_saw*."),
    write("The saw seems to be covered in rust, but it might be good for a single use."),
    nl,
    write("They may be useful later so you decide to take them."),
    nl,
    assert(have(electrical_tools)),
    assert(have(hand_saw)),
    retract(at(toolbox, workshop)),
    !.

inspect(workshop_window) :-
    write("You look at the window and into space. You see pieces of debris coming from the ship as well as some strange black round objects you can't identify"),
    nl,
    write("Can be broken with enough force. Last time this happened 2 workers got sucked out into space"),
    nl,
    !. 

inspect(small_fire) :- 
    write("A small electical fire seems to have broken out in the corner of the room"),
    nl,
    !.

inspect(table) :-
    write("An old wooden table. One of its legs seems to be barely holding on."),
    write("You might be able to detach it if you had the proper tool"),
    nl,
    !.

% Getting the table leg
use(hand_saw, table) :-
    have(hand_saw),
    i_am_at(workshop),
    at(table, workshop),
    write("You manage to sever the loose leg with your saw. The second the leg comes off the saw breaks."),
    retract(have(hand_saw)),
    retract(at(table, workshop)),
    write("You take the *wooden_table_leg*"),
    assert(have(wooden_table_leg)),
    nl,
    !.


use(wooden_table_leg, small_fire) :-
    write("You put the table leg near the fire and wait for the end of it to catch on fire"),
    nl,
    write("You create a *makeshift_torch*"),
    retract(have(wooden_table_leg)),
    assert(have(makeshift_torch)),
    nl,
    !.


use(makeshift_torch, alien_mass) :-
    clear_escape_pod_path,
    nl,
    !.

use(makeshift_torch, black_sludge) :-
    clear_escape_pod_path,
    nl,
    !.

clear_escape_pod_path :-
    write("You set the alien mass on fire, clearing the path south into the escape pod bay"), % TODO IMPROVE DESCRIPTION
    nl,
    write("The mass slowly burns away, leaving only the partly-digested corpse of one of your coworkers"),
    nl,
    retract(at(alien_mass, workshop)),
    retract(have(makeshift_torch)),
    nl,
    assert(path(workshop, s, escape_pods)),
    !.


use(engineering_chief_access_card, engineering_chief_office_door) :-
    have(engineering_chief_access_card),
    locked(engineering_chief_office_door),
    retract(locked(engineering_chief_office_door)),
    write("You slide the card through the reader and the door opens automatically, "),
    write("revealing an office in dissaray"),
    assert(path(workshop, n, engineering_chief_office)),
    !.


use(hammer, workshop_window) :-
    write("The hammer bounces off the reinforced glass. You're going to need something heavier"),
    nl,
    !.

use(metal_statue, workshop_window) :-
    have(space_suit),
    write("You hurl the statue at the window, breaking it."),
    write("The air begins to get sucked out the room at an incredible speed. The fire goes out."),
    write("You quickly grab onto the nearest pipe and the space suit lets you survive the pressure drop and lack of oxygen"),
    write("All loose objects in the room fly out of the window"), 
    write("and the alien mass gets sucked out with them, leaving the path south clear."),
    assert(path(workshop, s, escape_pods)),
    retract(at(workshop_window, workshop)),
    retract(have(metal_statue)),
    retract(at(small_fire, workshop)),
    nl,
    !.

use(metal_statue, workshop_window) :-
    retract(at(workshop_window, workshop)),
    retract(have(metal_statue)),
    write("You hurl the statue at the window, breaking it."),
    write("The air begins to get sucked out the room at an incredible speed."),
    write("You lack the proper equipment to surivive without oxygen and begin to lose consciousness"),
    die, nl,
    !.
% Chief's office
path(engineering_chief_office, s, workshop).
at(computer, engineering_chief_office).
at(metal_statue, engineering_chief_office).
pickable(metal_statue).

inspect(computer) :-
    write("You open the computer sitting on the desk."), nl,
    write("You find an open email titled ESCAPE POD CODE UPDATE: "), nl,
    write("Hi, Qaux\'ods, please remember about the annual escape pod tests."),nl,
    write("We've changed all the codes to *1867* for this week to make the process easier. Please have the report done by next week. Cheers."),
    nl,
    !. 

inspect(metal_statue) :-
    write("A heavy metal statue seems to have fallen down from one of the shelves and broken through a glass table."),
    nl,
    write("It's just small enough for you to pick up and seems to be some kind of award given to the engineering chief."),
    nl,
    !. 



describe(engineering_chief_office) :-
    write("The office is in heavy dissaray. An open computer sits on the desk. Next to one of the bookshelves lays a broken glass table. "),
    nl,
    write("Something heavy must've fallen on it from one of the shelves."),
    nl, !.



%escape pod room with broken control console for lowering 
%the pods and escape pods that require a launch key (from chief's computer).
% after that the game ends and the player wins. 

path(escape_pods, n, workshop).

at(broken_console, escape_pods).


describe(escape_pods) :-
    write("This room is designed to hold the emergency evacuation modules for the engineering staff."),
    nl,
    write("All of them have either already been deployed, or are now covered in an alien, dark grey substance similar to the one that blocked the entrance to this room."),
    nl,
    write("All except for one. You have to move fast."),
    nl,
    write("The pods must first be lowered using the console."),
    write("Then, once inside one of the pods, access to launch has to be granted by entering a code known to the managers of a given branch of the station."),
    nl,
    !. 

use(electrical_tools, broken_console) :-
    write("You manage to fix the console and use it to lower down the remaining escape pod"),
    nl,
    write("You can now access the *escape_pod_launch_console* inside the pod and get out of here."),
    nl,
    assert(at(escape_pod_launch_console, escape_pods)),
    retract(at(broken_console, escape_pods)),
    !,
    nl.

inspect(broken_console) :-
    write("A console used for lowering the escape pods, broken. Looks like it short-circuted."),
    nl,
    write("You spot some black matter between the wires. This must be what caused the break."),
    nl,
    write("Needs specialised tools to be fixed"),
    nl,
    !.

inspect(escape_pod_launch_console) :-
    write("Inside the pod is a big screen with a prompt that reads: "),
    nl,
    write("PLEASE ENTER LAUNCH AUTHORISATION CODE TO INITIATE LAUNCH SEQUENCE"),
    nl,
    nl,
    !,
    nl.

type_code(escape_pod_launch_console, 1867) :- 
    write("You punch in the code. The door to the pod closes behind you and you hear a robotic voice come from the console:"),
    nl,
    write("Voice: Launch sequence initiated. Please take a seat and fasten your seatbelts."),
    nl,
    write("You sit down and hope for the best."),
    nl,
    write("After a 20 second countdown the pod begins to shake and propels itself out of the station."),
    nl, 
    nl,
    write("You made it. As you're leaving the station you see the ship is covered in a moving blanket of black material."),
    nl,
    write("You live to tell the tale. You try contacting the closest colony and explain the situation. You get permission for emergency landing."),
    nl,
    nl,
    write("Congratulations! You managed to escape the station!"),
    finish,
    !,
    nl. 

%end game





% =================michal===================================



use(X, Y):-
    i_am_at(Place),
    at(Y, Place),
    have(X),
    nl, write("You can\'t figure out how to use the "), write(X), write(" on the "),write(Y), write("."), nl,
    !, nl.

use(X, Y):-
    i_am_at(Place),
    at(Y, Place),
    nl, write("You don\'t have "), write(X), write("."), nl,
    !, nl.

use(_, Y):-
    nl, write("There is no "), write(Y), write(" here."), nl,
    !, nl.

inspect(_):-
    nl, write("It is what it is, nothing special about it."), nl,
    !, nl.

describe(_) :-
    nl.

open(_) :-
    write("cannot open "), nl.

