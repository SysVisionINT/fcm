%%
%% fcm - Firebase Cloud Messaging
%% 
%% Copyright (C) 2018 SysVision - Consultadoria e Desenvolvimento em Sistemas de Informática, Lda.  
%% 
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%%

%% ====================================================================
%% Constants
%% ====================================================================
-define(FCM_ANDROID_MESSAGE_PRIORITY_NORMAL, <<"normal">>).
-define(FCM_ANDROID_MESSAGE_PRIORITY_HIGH,   <<"high">>).

%% ====================================================================
%% Records
%% ====================================================================
-record(fcm_message, {data, notification, android, webpush, apns, token, topic, condition}).
-record(fcm_map_entry, {key, value}).
-record(fcm_notification, {title, body}).
-record(fcm_android_config, {collapse_key, priority, ttl, restricted_package_name, data, notification}).
-record(fcm_android_notification, {title, body, icon, color, sound, tag, click_action, body_loc_key, body_loc_args, title_loc_key, title_loc_args}).
-record(fcm_webpush_config, {headers, data, notification}).
-record(fcm_apns_config, {headers, payload}).
