import React, {Component} from 'react'
import Draggable from 'react-draggable'
import PropTypes from 'prop-types';
import styles from './Point.scss';

class Point extends Component{
  constructor(props) {
    super(props);

    this.state = {
      alteredPosition: {
        x: this.props.position.x - 10,
        y: this.props.position.y - 10,
      },
      clickable: true,
    }
    this.handleDrag = this.handleDrag.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  handleDrag(e, ui) {
    const {alteredPosition, actualPosition} = this.state;
    this.setState({
      clickable: false,
    });

    let position = {
        x: this.props.position.x + ui.deltaX,
        y: this.props.position.y,
      }

    if(!this.props.anchor)
      position.y = position.y + ui.deltaY;
    this.props.handleOnDrag(position, this.props.id);
  }

  handleClick() {
    if (this.state.clickable === false) {
      this.setState({
        clickable: true
      })
    }
    else {
      this.props.handleOnClick();
    }
  }

  render() {
    const { color, active, anchor} = this.props;
    const {x, y} = this.state.alteredPosition;
    return (
      <Draggable defaultPosition={this.state.alteredPosition} onDrag={this.handleDrag} axis={anchor ? "x" : "both"} bounds={{top: 0, left: 0, right: 800, bottom: 580}}>
        <svg className={styles.Point} width={20} height={20} onClick={this.handleClick}>
          <circle cx={10} cy={10} r={10} fill={color} fillOpacity={active ? 100 : 0}/>
        </svg>
      </Draggable>
    );
  }
}
Point.propTypes = {
  handleOnDrag: PropTypes.func.isRequired,
  handleOnClick: PropTypes.func.isRequired,
  id: PropTypes.number.isRequired,
  position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y:PropTypes.number.isRequired,
            }).isRequired,
  anchor: PropTypes.bool.isRequired,
  color: PropTypes.string.isRequired,
  active: PropTypes.bool.isRequired,
}
export default Point;
