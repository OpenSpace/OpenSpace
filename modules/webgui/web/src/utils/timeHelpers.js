import {SetDeltaTimeScript, TogglePauseScript, ValuePlaceholder} from "../api/keys";
import DataManager from "../api/DataManager";

export const togglePause = () => {
  DataManager.runScript(TogglePauseScript);
};

export const realtime = () => {
  const script = SetDeltaTimeScript.replace(ValuePlaceholder, 1);
  DataManager.runScript(script);
};

export const now = () => {
  setDate(new Date());
  UpdateDeltaTimeNow(1);
};

// Spice, that is handling the time parsing in OpenSpace does not support
// ISO 8601-style time zones (the Z). It does, however, always assume that UTC
// is given.
export const setDate = (time) => {
  const fixedTimeString = time.toJSON().replace('Z', '');
  DataManager.setValue('__time', fixedTimeString);
};

/**
 * Make sure the date string contains a time zone
 * @param date
 * @param zone - the time zone in ISO 8601 format
 * @constructor
 */
export const DateStringWithTimeZone = (date, zone = 'Z') => {
  return (!date.includes('Z') ? `${date}${zone}` : date);
};

export const UpdateDeltaTimeNow = (value) => {
  const script = SetDeltaTimeScript.replace(ValuePlaceholder, value);
  DataManager.runScript(script);
};

export const sortDates = (dateList) => {
  dateList.sort((date1, date2) => (new Date(date1.date) - (new Date(date2.date))));
};
