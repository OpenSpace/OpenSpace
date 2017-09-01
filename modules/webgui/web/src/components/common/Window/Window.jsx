import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Draggable from 'react-draggable';
import styles from './Window.scss';
import Icon from '../Icon/Icon';
import Button from '../Input/Button/Button';
import { excludeKeys } from '../../../utils/helpers';

class Window extends Component {
  constructor(props) {
    super(props);

    this.state = { position: props.position, size: props.size };
    this.dragPosition = { x: 0, y: 0 };
    this.onMove = this.onMove.bind(this);
    this.onStart = this.onStart.bind(this);
    this.onEnd = this.onEnd.bind(this);
  }

  onMove(event) {
    const diff = {
      x: event.pageX - this.dragPosition.x,
      y: event.pageY - this.dragPosition.y,
    };
    this.dragPosition = { x: event.pageX, y: event.pageY };
    const { position } = this.state;
    this.setState({
      position: {
        x: position.x + diff.x,
        y: position.y + diff.y,
      },
    });
  }

  onStart(event) {
    this.dragPosition = { x: event.pageX, y: event.pageY };
  }

  onEnd() {
    // this.dragPosition = { x: 0, y: 0 };
  }

  render() {
    const { children, title, closeCallback } = this.props;
    const { position, size } = this.state;
    return (
      <Draggable defaultPosition={position} handle=".header">
        <section
          className={styles.window}
          style={{
            width: `${size.width}px`,
            height: `${size.height}px`,
          }}
          {...excludeKeys(this.props, 'children title callback')}
        >
          <header className="header">
            { title }
            { closeCallback && (
              <Button onClick={closeCallback} transparent small>
                <Icon icon="close" className="small" />
              </Button>
            )}
          </header>
          <section className={styles.body}>
            { children }
          </section>
        </section>
      </Draggable>
    );
  }
}

const NumberPair = (a, b) => PropTypes.shape({
  [a]: PropTypes.number,
  [b]: PropTypes.number,
});

Window.propTypes = {
  children: PropTypes.node,
  closeCallback: PropTypes.func,
  position: NumberPair('x', 'y'),
  size: NumberPair('height', 'width'),
  title: PropTypes.string,
};

Window.defaultProps = {
  children: '',
  closeCallback: null,
  position: { x: 10, y: 10 },
  size: { height: 300, width: 300 },
  title: 'Window',
};

export default Window;
