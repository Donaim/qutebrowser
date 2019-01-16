# vim: ft=python fileencoding=utf-8 sts=4 sw=4 et:

# Copyright 2018 Florian Bruhin (The Compiler) <mail@qutebrowser.org>
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

"""Infrastructure for intercepting requests."""

import typing
import enum

import attr

MYPY = False
if MYPY:
    # pylint: disable=unused-import,useless-suppression
    from PyQt5.QtCore import QUrl


# Possible request types that can be received. Currently correspond to the
# QWebEngineUrlRequestInfo Enum:
# https://doc.qt.io/qt-5/qwebengineurlrequestinfo.html#ResourceType-enum
ResourceType = enum.Enum('ResourceType',
                         ['main_frame', 'sub_frame', 'stylesheet', 'script',
                          'image', 'font_resource', 'sub_resource', 'object',
                          'media', 'worker', 'shared_worker', 'prefetch',
                          'favicon', 'xhr', 'ping', 'service_worker',
                          'csp_report', 'plugin_resource', 'unknown'])


@attr.s
class Request:

    """A request which can be intercepted/blocked."""

    #: The URL of the page being shown.
    first_party_url = attr.ib()  # type: QUrl

    #: The URL of the file being requested.
    request_url = attr.ib()  # type: QUrl

    is_blocked = attr.ib(False)  # type: bool

    #: The resource type of the request. None if not supported on this backend.
    resource_type = attr.ib(None)  # type: typing.Optional[ResourceType]

    def block(self) -> None:
        """Block this request."""
        self.is_blocked = True


#: Type annotation for an interceptor function.
InterceptorType = typing.Callable[[Request], None]


_interceptors = []  # type: typing.List[InterceptorType]


def register(interceptor: InterceptorType) -> None:
    _interceptors.append(interceptor)


def run(info: Request) -> None:
    for interceptor in _interceptors:
        interceptor(info)
