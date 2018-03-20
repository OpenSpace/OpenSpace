// @flow
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { rotate } from '../../../utils/helpers';
import Icon from '../Icon/Icon';
import Button from '../Input/Button/Button';

import styles from './Calendar.scss';
import InlineInput from '../Input/InlineInput/InlineInput';

const Days = {
  Sunday: 0,
  Monday: 1,
  Tuesday: 2,
  Wednesday: 3,
  Thursday: 4,
  Friday: 5,
  Saturday: 6,
};
Object.freeze(Days);

const Months = 'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec'.split(' ');
Object.freeze(Months);

// how many days of previous month to get depending on weekday
const DaysInWeekBefore = [7, 1, 2, 3, 4, 5, 6];
// always display 6 weeks
const expectedDaysInCalendar = 6 * 7;

class Calendar extends Component {
  static daysOfMonth(month: Date) {
    const iterator = new Date(month.getTime());
    const days = [];

    while (iterator.getMonth() === month.getMonth()) {
      days.push(Calendar.copy(iterator));
      iterator.setDate(iterator.getDate() + 1);
    }

    return days;
  }

  static isSameDay(a: Date, b: Date): boolean {
    return a.getFullYear() === b.getFullYear() &&
      a.getMonth() === b.getMonth() &&
      a.getDate() === b.getDate();
  }

  /**
   * Expose the days object, so that it may be used as Calendar.Days.Sunday, for instance
   */
  static get Days(): Days {
    return Days;
  }

  /**
   * set the date component to 1
   * @param day - remains unchanged
   * @returns {Date}
   */
  static toStartOfMonth(day: Date): Date {
    const newDay = Calendar.copy(day);
    newDay.setDate(1);
    return newDay;
  }

  /**
   * copy date
   * @param date
   */
  static copy(date: Date): Date {
    return new Date(date.getTime());
  }

  constructor(props) {
    super(props);

    this.state = {
      activeMonth: Calendar.toStartOfMonth(props.activeMonth),
    };

    this.setCurrentMonth = this.setCurrentMonth.bind(this);
    this.setYear = this.setYear.bind(this);
  }

  state: { activeMonth: Date };

  componentWillReceiveProps({ activeMonth }) {
    // update calendar focus (unless user has moved away from previously given active month)
    if (Calendar.isSameDay(Calendar.toStartOfMonth(this.props.activeMonth),
                           Calendar.toStartOfMonth(this.state.activeMonth))) {
      this.setState({ activeMonth: Calendar.toStartOfMonth(activeMonth) });
    }
  }

  onSelect(day: Date): Function {
    return () => {
      this.props.onChange(day);
    };
  }

  get dayHeader(): Array<{ day: Date, index: number }> {
    return this.props.dayHeaders.map((day, index) => ({ day, index }));
  }

  get days(): Array<Date> {
    const prevMonth: Date = this.month(-1);
    const thisMonth: Date = this.month();
    const nextMonth: Date = this.month(1);

    const days = Calendar.daysOfMonth(thisMonth);
    const prev = Calendar.daysOfMonth(prevMonth);
    const next = Calendar.daysOfMonth(nextMonth);

    const daysFromPrevMonth: number = this.daysToGet(thisMonth.getDay());
    days.unshift(...prev.slice(-1 * daysFromPrevMonth));
    const daysFromNextMonth: number = expectedDaysInCalendar - days.length;
    days.push(...next.slice(0, daysFromNextMonth));

    return days;
  }

  daysToGet(day: number): number {
    const rotatedDays = rotate(DaysInWeekBefore, 7 - this.props.weekStartsOn);
    return rotatedDays[day];
  }

  month(add: number = 0): Date {
    const { activeMonth } = this.state;
    if (add === 0) {
      return activeMonth;
    }

    const newDate = new Date(activeMonth.getTime());
    newDate.setMonth(newDate.getMonth() + add);
    return newDate;
  }

  monthString(add: number = 0) {
    return this.props.months[this.month(add).getMonth()];
  }

  isThisMonth(day: Date): boolean {
    return day.getMonth() === this.month().getMonth();
  }

  isSelected(day: Date): boolean {
    if (!this.props.selected) {
      return false;
    }

    return Calendar.isSameDay(day, this.props.selected);
  }

  changeMonth(dir: number): Function {
    return () => this.setState({ activeMonth: this.month(dir) });
  }

  setCurrentMonth() {
    this.setState({ activeMonth: Calendar.toStartOfMonth(new Date()) });
  }

  setYear(event) {
    const { value } = event.currentTarget;
    const { activeMonth } = this.state;
    activeMonth.setFullYear(Number.parseInt(value, 10));
    this.setState({ activeMonth });
  }

  extraClasses(day: Date): string {
    let classes = '';

    if (!this.isThisMonth(day)) {
      classes += `${styles.faint} `;
    }

    if (this.isSelected(day)) {
      classes += `${styles.selected} `;
    }

    if (Calendar.isSameDay(day, new Date())) {
      classes += `${styles.today} `;
    }

    return classes;
  }

  render() {
    return (
      <section>
        <header className={styles.header}>
          <Button transparent small onClick={this.changeMonth(-1)}>
            <Icon icon="chevron_left" />
          </Button>
          <div>
            { this.props.todayButton && (
              <Button onClick={this.setCurrentMonth} title="Today" small transparent>
                <Icon icon="today" />
              </Button>
            )} {
              this.monthString()
            } <InlineInput
              type="number"
              value={this.month().getFullYear()}
              onChange={this.setYear}
              className={styles.year}
            />
          </div>
          <Button transparent small onClick={this.changeMonth(1)}>
            <Icon icon="chevron_right" />
          </Button>
        </header>

        <section className={styles.calendar}>
          <header className={styles.weekdays}>
            { this.dayHeader.map(d => (
              <div key={d.index} className={styles.weekday}>
                { d.day }
              </div>
            ))}
          </header>

          <section className={styles.month}>
            { this.days.map(day => (
              <div
                key={`${day.getMonth()}-${day.getDate()}`}
                className={`${styles.day} ${this.extraClasses(day)}`}
                onClick={this.onSelect(day)}
                role="button"
                tabIndex={0}
              >
                { day.getDate() }
              </div>
            ))}
          </section>
        </section>
      </section>
    );
  }
}

Calendar.propTypes = {
  activeMonth: PropTypes.instanceOf(Date),
  dayHeaders: PropTypes.arrayOf(PropTypes.string),
  months: PropTypes.arrayOf(PropTypes.string),
  onChange: PropTypes.func,
  selected: PropTypes.instanceOf(Date),
  todayButton: PropTypes.bool,
  weekStartsOn: PropTypes.number,
};

Calendar.defaultProps = {
  activeMonth: new Date(),
  dayHeaders: 'M T W T F S S'.split(' '),
  months: Months,
  onChange: () => {},
  selected: null,
  todayButton: false,
  weekStartsOn: Calendar.Days.Monday,
};

export default Calendar;
