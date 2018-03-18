import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Button from '../Button/Button';
import InlineInput from '../InlineInput/InlineInput';
import styles from './Time.scss';
import Icon from '../../Icon/Icon';

const Elements = {
  Hours: 'hours',
  Minutes: 'minutes',
  Seconds: 'seconds',
  Milliseconds: 'milliseconds',
};
Elements.Normal = [Elements.Hours, Elements.Minutes, Elements.Seconds];
Elements.All = [Elements.Hours, Elements.Minutes, Elements.Seconds, Elements.Milliseconds];
Object.freeze(Elements);

class Time extends Component {
  static zeroPad(number) {
    return number < 10 ? `0${number}` : number;
  }

  constructor(props) {
    super(props);
    this.state = { time: props.time };

    this.onClick = this.onClick.bind(this);
    this.onInput = this.onInput.bind(this);
  }

  componentWillReceiveProps({ time }) {
    this.setState({ time });
  }

  onClick(what, change) {
    const getterFunc = `get${what}`;
    const setterFunc = `set${what}`;

    return () => {
      const { time } = this.state;
      time[setterFunc](time[getterFunc]() + change);

      this.setState({ time });
      if (this.hasCallback) {
        this.props.onChange(time);
      }
    };
  }

  onInput(what) {
    const setterFunc = `set${what}`;

    return (event) => {
      const { time } = this.state;
      const { value } = event.currentTarget;
      time[setterFunc](Number.parseFloat(value));
      this.setState({ time });
      if (this.hasCallback) {
        this.props.onChange(time);
      }
    };
  }

  get hours() {
    const { time } = this.state;
    const minutes = this.shouldInclude(Elements.Minutes);
    return this.wrap(`${Time.zeroPad(time.getHours())}`, 'Hours', minutes && ':');
  }

  get minutes() {
    const { time } = this.state;
    const seconds = this.shouldInclude(Elements.Seconds);
    return this.wrap(`${Time.zeroPad(time.getMinutes())}`, 'Minutes', seconds && ':');
  }

  get seconds() {
    const { time } = this.state;
    const milliseconds = this.shouldInclude(Elements.Milliseconds);
    return this.wrap(`${Time.zeroPad(time.getSeconds())}`, 'Seconds', milliseconds && '.');
  }

  get milliseconds() {
    const { time } = this.state;
    return this.wrap(time.getMilliseconds(), 'Milliseconds');
  }

  get hasCallback() {
    return this.props.onChange !== null;
  }

  shouldInclude(what) {
    return this.props.elements.includes(what);
  }

  /**
   * wrap the time component with the needed elements
   * @param inner - the time itself
   * @param what - what element is this? hours? seconds?
   * @param after
   * @returns {XML}
   */
  wrap(inner, what, after = '') {
    // make it editable with input and such?
    if (this.hasCallback) {
      const width = what === 'Milliseconds' ? 3 : 2;
      return (
        <div className={styles.element}>
          <Button nopadding transparent onClick={this.onClick(what, 1)}>
            <Icon icon="expand_less" />
          </Button>
          <span>
            <InlineInput
              value={inner}
              size={width}
              className={styles.textInput}
              onChange={this.onInput(what)}
              type="number"
            />
          </span>
          <Button nopadding transparent onClick={this.onClick(what, -1)}>
            <Icon icon="expand_more" />
          </Button>
        </div>
      );
    }

    return (
      <div className={styles.element}>
        { inner }{after}
      </div>
    );
  }

  render() {
    return (
      <div className={styles.clock}>
        { this.shouldInclude(Elements.Hours) && this.hours }
        { this.shouldInclude(Elements.Minutes) && this.minutes }
        { this.shouldInclude(Elements.Seconds) && this.seconds }
        { this.shouldInclude(Elements.Milliseconds) && this.milliseconds }
      </div>
    );
  }
}

Time.Elements = Elements;

Time.propTypes = {
  /**
   * decide which element should be shown - should be an array of the elements in Elements
   */
  elements: PropTypes.arrayOf(PropTypes.string),
  onChange: PropTypes.func,
  time: PropTypes.instanceOf(Date).isRequired,
};

Time.defaultProps = {
  elements: Elements.Normal,
  onChange: null,
};

export default Time;
