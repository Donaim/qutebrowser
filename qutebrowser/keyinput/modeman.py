# vim: ft=python fileencoding=utf-8 sts=4 sw=4 et:

# Copyright 2014-2020 Florian Bruhin (The Compiler) <mail@qutebrowser.org>
#
# This file is part of qutebrowser.
#
# qutebrowser is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# qutebrowser is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with qutebrowser.  If not, see <http://www.gnu.org/licenses/>.

"""Mode manager singleton which handles the current keyboard mode."""

import functools
import typing

import attr
from PyQt5.QtCore import pyqtSlot, pyqtSignal, Qt, QObject, QEvent
from PyQt5.QtGui import QKeyEvent
from PyQt5.QtWidgets import QApplication
from PyQt5.QtGui import QKeyEvent

from qutebrowser.commands import runners
from qutebrowser.keyinput import modeparsers, basekeyparser
from qutebrowser.config import config
from qutebrowser.api import cmdutils
from qutebrowser.utils import usertypes, log, objreg, utils
from qutebrowser.browser import hints

INPUT_MODES = [usertypes.KeyMode.insert, usertypes.KeyMode.passthrough]
PROMPT_MODES = [usertypes.KeyMode.prompt, usertypes.KeyMode.yesno, usertypes.KeyMode.command]

ParserDictType = typing.MutableMapping[
    usertypes.KeyMode, basekeyparser.BaseKeyParser]


@attr.s(frozen=True)
class KeyEvent:

    """A small wrapper over a QKeyEvent storing its data.

    This is needed because Qt apparently mutates existing events with new data.
    It doesn't store the modifiers because they can be different for a key
    press/release.

    Attributes:
        key: A Qt.Key member (QKeyEvent::key).
        text: A string (QKeyEvent::text).
    """

    key = attr.ib()  # type: Qt.Key
    text = attr.ib()  # type: str

    @classmethod
    def from_event(cls, event: QKeyEvent) -> 'KeyEvent':
        """Initialize a KeyEvent from a QKeyEvent."""
        return cls(Qt.Key(event.key()), event.text())


class NotInModeError(Exception):

    """Exception raised when we want to leave a mode we're not in."""


def init(win_id: int, parent: QObject) -> 'ModeManager':
    """Initialize the mode manager and the keyparsers for the given win_id."""
    modeman = ModeManager(win_id, parent)
    objreg.register('mode-manager', modeman, scope='window', window=win_id)

    commandrunner = runners.CommandRunner(win_id)

    hintmanager = hints.HintManager(win_id, parent=parent)
    objreg.register('hintmanager', hintmanager, scope='window',
                    window=win_id, command_only=True)

    keyparsers = {
        usertypes.KeyMode.normal:
            modeparsers.NormalKeyParser(
                win_id=win_id,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.hint:
            modeparsers.HintKeyParser(
                win_id=win_id,
                commandrunner=commandrunner,
                hintmanager=hintmanager,
                parent=modeman),

        usertypes.KeyMode.insert:
            modeparsers.PassthroughKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.insert,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.passthrough:
            modeparsers.PassthroughKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.passthrough,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.command:
            modeparsers.PassthroughKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.command,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.prompt:
            modeparsers.PassthroughKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.prompt,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.yesno:
            modeparsers.PromptKeyParser(
                win_id=win_id,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.caret:
            modeparsers.CaretKeyParser(
                win_id=win_id,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.set_mark:
            modeparsers.RegisterKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.set_mark,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.jump_mark:
            modeparsers.RegisterKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.jump_mark,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.record_macro:
            modeparsers.RegisterKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.record_macro,
                commandrunner=commandrunner,
                parent=modeman),

        usertypes.KeyMode.run_macro:
            modeparsers.RegisterKeyParser(
                win_id=win_id,
                mode=usertypes.KeyMode.run_macro,
                commandrunner=commandrunner,
                parent=modeman),
    }  # type: ParserDictType

    for mode, parser in keyparsers.items():
        modeman.register(mode, parser)

    return modeman


def instance(win_id: typing.Union[int, str]) -> 'ModeManager':
    """Get a modemanager object."""
    return objreg.get('mode-manager', scope='window', window=win_id)


def enter(win_id: int,
          mode: usertypes.KeyMode,
          reason: str = None,
          only_if_normal: bool = False) -> None:
    """Enter the mode 'mode'."""
    instance(win_id).enter(mode, reason, only_if_normal)


def leave(win_id: int,
          mode: usertypes.KeyMode,
          reason: str = None, *,
          maybe: bool = False) -> None:
    """Leave the mode 'mode'."""
    instance(win_id).leave(mode, reason, maybe=maybe)


class ModeManager(QObject):

    """Manager for keyboard modes.

    Attributes:
        mode: The mode we're currently in.
        _win_id: The window ID of this ModeManager
        _prev_mode: Mode before a prompt popped up
        parsers: A dictionary of modes and their keyparsers.
        _forward_unbound_keys: If we should forward unbound keys.
        _releaseevents_to_pass: A set of KeyEvents where the keyPressEvent was
                                passed through, so the release event should as
                                well.

    Signals:
        entered: Emitted when a mode is entered.
                 arg1: The mode which has been entered.
                 arg2: The window ID of this mode manager.
        left:  Emitted when a mode is left.
                 arg1: The mode which has been left.
                 arg2: The new current mode.
                 arg3: The window ID of this mode manager.
    """

    entered = pyqtSignal(usertypes.KeyMode, int)
    left = pyqtSignal(usertypes.KeyMode, usertypes.KeyMode, int)

    def __init__(self, win_id: int, parent: QObject = None) -> None:
        super().__init__(parent)
        self._win_id = win_id
        self.parsers = {}  # type: ParserDictType
        self._prev_mode = usertypes.KeyMode.normal
        self.mode = usertypes.KeyMode.normal
        self._releaseevents_to_pass = set()  # type: typing.Set[KeyEvent]

    def __repr__(self) -> str:
        return utils.get_repr(self, mode=self.mode)

    def _handle_keypress(self, event: QKeyEvent, *,
                         dry_run: bool = False) -> bool:
        """Handle filtering of KeyPress events.

        Args:
            event: The KeyPress to examine.
            dry_run: Don't actually handle the key, only filter it.

        Return:
            True if event should be filtered, False otherwise.
        """
        curmode = self.mode
        parser = self.parsers[curmode]
        if curmode != usertypes.KeyMode.insert:
            log.modes.debug("got keypress in mode {} - delegating to "
                            "{}".format(curmode, utils.qualname(parser)))

        isShifted = bool(event.modifiers() & Qt.ShiftModifier)

        # TODO: make a script for generating this
        native_codes = {
            (24, False): (81, "q"),
            (25, False): (87, "w"),
            (26, False): (69, "e"),
            (27, False): (82, "r"),
            (28, False): (84, "t"),
            (29, False): (89, "y"),
            (30, False): (85, "u"),
            (31, False): (73, "i"),
            (31, False): (73, "i"),
            (32, False): (79, "o"),
            (33, False): (80, "p"),
            (34, False): (91, "["),
            (35, False): (93, "]"),
            (38, False): (65, "a"),
            (39, False): (83, "s"),
            (40, False): (68, "d"),
            (41, False): (70, "f"),
            (42, False): (71, "g"),
            (43, False): (72, "h"),
            (44, False): (74, "j"),
            (45, False): (75, "k"),
            (46, False): (76, "l"),
            (47, False): (59, ";"),
            (48, False): (39, "'"),
            (52, False): (90, "z"),
            (53, False): (88, "x"),
            (54, False): (67, "c"),
            (55, False): (86, "v"),
            (56, False): (66, "b"),
            (57, False): (78, "n"),
            (58, False): (77, "m"),
            (59, False): (44, ","),
            (60, False): (46, "."),
            (61, False): (47, "/"),
            (24, True): (81, "Q"),
            (25, True): (87, "W"),
            (26, True): (69, "E"),
            (27, True): (82, "R"),
            (28, True): (84, "T"),
            (29, True): (89, "Y"),
            (30, True): (85, "U"),
            (31, True): (73, "I"),
            (32, True): (79, "O"),
            (33, True): (80, "P"),
            (34, True): (123,"{"),
            (35, True): (125,"}"),
            (38, True): (65, "A"),
            (39, True): (83, "S"),
            (40, True): (68, "D"),
            (41, True): (70, "F"),
            (42, True): (71, "G"),
            (43, True): (72, "H"),
            (44, True): (74, "J"),
            (45, True): (75, "K"),
            (46, True): (76, "L"),
            (47, True): (58, ":"),
            (48, True): (34, "\""),
            (52, True): (90, "Z"),
            (53, True): (88, "X"),
            (54, True): (67, "C"),
            (55, True): (86, "V"),
            (56, True): (66, "B"),
            (57, True): (78, "N"),
            (58, True): (77, "M"),
            (59, True): (60, "<"),
            (60, True): (62, ">"),
            (61, True): (63, "?"),
        }

        dkey = (event.nativeScanCode(), isShifted)
        if dkey in native_codes:
            (newcode, text) = native_codes[dkey]
            if newcode != event.key():
                log.modes.debug("key {} replaced by {}".format(event.text(), text))
                event = QKeyEvent(event.type(), newcode, event.modifiers(), text)

        match = parser.handle(event, dry_run=dry_run)

        is_non_alnum = (
            event.modifiers() not in [Qt.NoModifier, Qt.ShiftModifier] or
            not event.text().strip())

        forward_unbound_keys = config.cache['input.forward_unbound_keys']

        if match:
            filter_this = True
        elif (parser.passthrough or forward_unbound_keys == 'all' or
              (forward_unbound_keys == 'auto' and is_non_alnum)):
            filter_this = False
        else:
            filter_this = True

        if not filter_this and not dry_run:
            self._releaseevents_to_pass.add(KeyEvent.from_event(event))

        if curmode != usertypes.KeyMode.insert:
            focus_widget = QApplication.instance().focusWidget()
            log.modes.debug("match: {}, forward_unbound_keys: {}, "
                            "passthrough: {}, is_non_alnum: {}, dry_run: {} "
                            "--> filter: {} (focused: {!r})".format(
                                match, forward_unbound_keys,
                                parser.passthrough, is_non_alnum, dry_run,
                                filter_this, focus_widget))
        return filter_this

    def _handle_keyrelease(self, event: QKeyEvent) -> bool:
        """Handle filtering of KeyRelease events.

        Args:
            event: The KeyPress to examine.

        Return:
            True if event should be filtered, False otherwise.
        """
        # handle like matching KeyPress
        keyevent = KeyEvent.from_event(event)
        if keyevent in self._releaseevents_to_pass:
            self._releaseevents_to_pass.remove(keyevent)
            filter_this = False
        else:
            filter_this = True
        if self.mode != usertypes.KeyMode.insert:
            log.modes.debug("filter: {}".format(filter_this))
        return filter_this

    def register(self, mode: usertypes.KeyMode,
                 parser: basekeyparser.BaseKeyParser) -> None:
        """Register a new mode."""
        assert parser is not None
        self.parsers[mode] = parser
        parser.request_leave.connect(self.leave)

    def enter(self, mode: usertypes.KeyMode,
              reason: str = None,
              only_if_normal: bool = False) -> None:
        """Enter a new mode.

        Args:
            mode: The mode to enter as a KeyMode member.
            reason: Why the mode was entered.
            only_if_normal: Only enter the new mode if we're in normal mode.
        """
        if mode == usertypes.KeyMode.normal:
            self.leave(self.mode, reason='enter normal: {}'.format(reason))
            return
        if not isinstance(mode, usertypes.KeyMode):
            raise TypeError("Mode {} is no KeyMode member!".format(mode))

        log.modes.debug("Entering mode {}{}".format(
            mode, '' if reason is None else ' (reason: {})'.format(reason)))
        if mode not in self.parsers:
            raise ValueError("No keyparser for mode {}".format(mode))
        if self.mode != usertypes.KeyMode.normal:
            if only_if_normal:
                log.modes.debug("Ignoring request as we're in mode {} "
                                "and only_if_normal is set..".format(
                                    self.mode))
                return
            log.modes.debug("Overriding mode {}.".format(self.mode))
            self.left.emit(self.mode, mode, self._win_id)

        if not self.mode in PROMPT_MODES:
            self._prev_mode = self.mode

        self.mode = mode
        self.entered.emit(mode, self._win_id)

    @cmdutils.register(instance='mode-manager', scope='window')
    def enter_mode(self, mode: str) -> None:
        """Enter a key mode.

        Args:
            mode: The mode to enter. See `:help bindings.commands` for the
                  available modes, but note that hint/command/yesno/prompt mode
                  can't be entered manually.
        """
        try:
            m = usertypes.KeyMode[mode]
        except KeyError:
            raise cmdutils.CommandError("Mode {} does not exist!".format(mode))

        if m in [usertypes.KeyMode.hint, usertypes.KeyMode.command,
                 usertypes.KeyMode.yesno, usertypes.KeyMode.prompt]:
            raise cmdutils.CommandError(
                "Mode {} can't be entered manually!".format(mode))

        self.enter(m, 'command')

    @pyqtSlot(usertypes.KeyMode, str, bool)
    def leave(self, mode: usertypes.KeyMode,
              reason: str = None,
              maybe: bool = False) -> None:
        """Leave a key mode.

        Args:
            mode: The mode to leave as a usertypes.KeyMode member.
            reason: Why the mode was left.
            maybe: If set, ignore the request if we're not in that mode.
        """
        if self.mode != mode:
            if maybe:
                log.modes.debug("Ignoring leave request for {} (reason {}) as "
                                "we're in mode {}".format(
                                    mode, reason, self.mode))
                return
            else:
                raise NotInModeError("Not in mode {}!".format(mode))

        log.modes.debug("Leaving mode {}{}".format(
            mode, '' if reason is None else ' (reason: {})'.format(reason)))
        # leaving a mode implies clearing keychain, see
        # https://github.com/qutebrowser/qutebrowser/issues/1805
        self.clear_keychain()
        self.mode = usertypes.KeyMode.normal
        self.left.emit(mode, self.mode, self._win_id)
        self.enter(self._prev_mode,
                   reason='restore mode before {}'.format(mode.name))

    @cmdutils.register(instance='mode-manager', name='leave-mode',
                       not_modes=[usertypes.KeyMode.normal], scope='window')
    def leave_current_mode(self) -> None:
        """Leave the mode we're currently in."""
        if self.mode == usertypes.KeyMode.normal:
            raise ValueError("Can't leave normal mode!")
        self.leave(self.mode, 'leave current')

    def handle_event(self, event: QEvent) -> bool:
        """Filter all events based on the currently set mode.

        Also calls the real keypress handler.

        Args:
            event: The KeyPress to examine.

        Return:
            True if event should be filtered, False otherwise.
        """
        handlers = {
            QEvent.KeyPress: self._handle_keypress,
            QEvent.KeyRelease: self._handle_keyrelease,
            QEvent.ShortcutOverride:
                functools.partial(self._handle_keypress, dry_run=True),
        }  # type: typing.Mapping[QEvent.Type, typing.Callable[[QEvent], bool]]
        handler = handlers[event.type()]
        return handler(event)

    @cmdutils.register(instance='mode-manager', scope='window')
    def clear_keychain(self) -> None:
        """Clear the currently entered key chain."""
        self.parsers[self.mode].clear_keystring()
